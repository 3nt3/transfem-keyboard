#![no_std]
#![no_main]

mod fmt;

use core::sync::atomic::{AtomicBool, AtomicU8, Ordering};

use defmt::Format;
use embassy_futures::join::join;
use embassy_sync::{blocking_mutex::raw::CriticalSectionRawMutex, channel::Channel};
use embassy_usb::{
    class::hid::{self, HidReaderWriter, ReportId, RequestHandler, State},
    control::OutResponse,
    Builder, Handler,
};
#[cfg(not(feature = "defmt"))]
use panic_halt as _;
use usbd_hid::{
    descriptor::{KeyboardReport, SerializedDescriptor},
    hid_class::HidProtocolMode,
};
#[cfg(feature = "defmt")]
use {defmt_rtt as _, panic_probe as _};

use embassy_executor::Spawner;
use embassy_stm32::{
    bind_interrupts,
    exti::ExtiInput,
    gpio::{Level, Output, Pull, Speed},
    peripherals,
    time::Hertz,
    usb::{self, Driver},
    Config,
};
use embassy_time::{Duration, Timer};
use fmt::info;

bind_interrupts!(struct Irqs {
    USB_LP_CAN1_RX0 => usb::InterruptHandler<peripherals::USB>;
});

struct ButtonConfig {
    button: ExtiInput<'static>,
    keycode: Option<u8>,
    modifier: Option<u8>,
}

enum KeyEvent {
    Pressed((Option<u8>, Option<u8>)),
    Released((Option<u8>, Option<u8>)),
}

impl Format for KeyEvent {
    fn format(&self, fmt: defmt::Formatter) {
        match self {
            KeyEvent::Pressed((code, modifier)) => {
                defmt::write!(fmt, "Pressed({}, {})", code, modifier)
            }
            KeyEvent::Released((code, modifier)) => {
                defmt::write!(fmt, "Released({}, {})", code, modifier)
            }
        }
    }
}

type KeyChannel = Channel<CriticalSectionRawMutex, KeyEvent, 4>;
static CHANNEL: KeyChannel = KeyChannel::new();

static HID_PROTOCOL_MODE: AtomicU8 = AtomicU8::new(HidProtocolMode::Boot as u8);

#[embassy_executor::main]
async fn main(spawner: Spawner) {
    let mut config = Config::default();
    {
        use embassy_stm32::rcc::*;
        config.rcc.hse = Some(Hse {
            freq: Hertz(8_000_000),
            mode: HseMode::Oscillator,
        });

        config.rcc.pll = Some(Pll {
            src: PllSource::HSE,
            prediv: PllPreDiv::DIV1,
            mul: PllMul::MUL9,
        });
        config.rcc.sys = Sysclk::PLL1_P;
        config.rcc.ahb_pre = AHBPrescaler::DIV1;
        config.rcc.apb1_pre = APBPrescaler::DIV2;
        config.rcc.apb2_pre = APBPrescaler::DIV1;
    }
    let mut p = embassy_stm32::init(config);

    {
        // BluePill board has a pull-up resistor on the D+ line.
        // Pull the D+ pin down to send a RESET condition to the USB bus.
        // This forced reset is needed only for development, without it host
        // will not reset your device when you upload new firmware.
        let _dp = Output::new(p.PA12.reborrow(), Level::High, Speed::Low);
        Timer::after_millis(10).await;
    }

    info!("Hello, World!");

    let driver = Driver::new(p.USB, Irqs, p.PA12, p.PA11);

    // Create embassy-usb config
    let mut config = embassy_usb::Config::new(0xc0de, 0xcafe);
    config.manufacturer = Some("3nt3");
    config.product = Some("Transfem Keyboard :3");
    config.composite_with_iads = false;
    config.max_packet_size_0 = 64;
    config.device_class = 1;
    config.device_sub_class = 0;
    config.device_protocol = 0;

    let mut config_descriptor = [0; 256];
    let mut bos_descriptor = [0; 256];
    let mut msos_descriptor = [0; 256];
    let mut control_buf = [0; 128];

    let mut request_handler = MyRequestHandler {};
    let mut device_handler = MyDeviceHandler::new();

    let mut state = State::new();

    let mut builder = Builder::new(
        driver,
        config,
        &mut config_descriptor,
        &mut bos_descriptor,
        &mut msos_descriptor,
        &mut control_buf,
    );

    builder.handler(&mut device_handler);

    let hid_config = hid::Config {
        report_descriptor: KeyboardReport::desc(),
        request_handler: None,
        poll_ms: 60,
        max_packet_size: 64,
    };

    let hid = HidReaderWriter::<_, 1, 8>::new(&mut builder, &mut state, hid_config);

    let mut usb = builder.build();

    let usb_fut = usb.run();

    let (reader, mut writer) = hid.split();

    // BUTTON GPIOs
    let mut shift_btn = ExtiInput::new(p.PB3, p.EXTI3, Pull::Up);
    let mut colon_btn = ExtiInput::new(p.PB4, p.EXTI4, Pull::Up);
    let mut three_btn = ExtiInput::new(p.PB5, p.EXTI5, Pull::Up);

    let buttons = [
        ButtonConfig {
            button: shift_btn,
            keycode: None,
            modifier: Some(0x02), // left shift
        },
        ButtonConfig {
            button: colon_btn,
            keycode: Some(0x33), // Colon (:) (actually semicolon)
            modifier: None,
        },
        ButtonConfig {
            button: three_btn,
            keycode: Some(0x20), // 3
            modifier: None,
        },
    ];

    let sender = CHANNEL.sender();
    let receiver = CHANNEL.receiver();

    for cfg in buttons {
        let sender = sender.clone();

        spawner
            .spawn(button_task(cfg.button, cfg.keycode, cfg.modifier, sender))
            .unwrap();
    }

    let writer_fut = async {
        let mut keys: [u8; 6] = [0; 6];
        let mut modifier: u8 = 0;

        loop {
            let event = receiver.receive().await;
            info!("Event: {:?}", event);

            match event {
                KeyEvent::Pressed((code, mod_val)) => {
                    // Handle keycode
                    if let Some(code) = code {
                        if !keys.contains(&code) {
                            if let Some(slot) = keys.iter_mut().find(|s| **s == 0) {
                                *slot = code;
                            }
                        }
                    }
                    // Handle modifier
                    if let Some(new_modifier) = mod_val {
                        modifier |= new_modifier;
                    }
                }
                KeyEvent::Released((code, mod_val)) => {
                    // Handle keycode
                    if let Some(code) = code {
                        for k in &mut keys {
                            if *k == code {
                                *k = 0;
                            }
                        }
                    }
                    // Handle modifier
                    if let Some(new_modifier) = mod_val {
                        modifier &= !new_modifier;
                    }
                }
            }

            // Decide boot vs report protocol
            if HID_PROTOCOL_MODE.load(Ordering::Relaxed) == HidProtocolMode::Boot as u8 {
                let mut boot: [u8; 8] = [0; 8];
                boot[2..8].copy_from_slice(&keys);
                boot[0] = modifier;
                writer.write(&boot).await.ok();
            } else {
                let report = KeyboardReport {
                    keycodes: keys,
                    leds: 0,
                    modifier,
                    reserved: 0,
                };
                writer.write_serialize(&report).await.ok();
            }
        }
    };

    let out_fut = async {
        reader.run(false, &mut request_handler).await;
    };

    // future that spams 'a'
    let a_fut = async {
        loop {
            // FIXME: just for debuggin

            info!("sending ':3\n'");
            send_text(":3\n", &sender).await;

            Timer::after(Duration::from_millis(500)).await;
        }
    };

    join(usb_fut, join(join(a_fut, writer_fut), out_fut)).await;

    loop {}
}

#[embassy_executor::task(pool_size = 4)]
async fn button_task(
    mut button: ExtiInput<'static>,
    keycode: Option<u8>,
    modifier: Option<u8>,
    sender: embassy_sync::channel::Sender<'static, CriticalSectionRawMutex, KeyEvent, 4>,
) -> ! {
    loop {
        button.wait_for_rising_edge().await;
        sender.send(KeyEvent::Pressed((keycode, modifier))).await;

        button.wait_for_falling_edge().await;
        sender.send(KeyEvent::Released((keycode, modifier))).await;
    }
}

struct MyRequestHandler {}

impl RequestHandler for MyRequestHandler {
    fn get_report(&mut self, id: ReportId, _buf: &mut [u8]) -> Option<usize> {
        info!("Get report for {:?}", id);
        None
    }

    fn set_report(&mut self, id: ReportId, data: &[u8]) -> OutResponse {
        info!("Set report for {:?}: {=[u8]}", id, data);
        OutResponse::Accepted
    }

    fn set_idle_ms(&mut self, id: Option<ReportId>, dur: u32) {
        info!("Set idle rate for {:?} to {:?}", id, dur);
    }

    fn get_idle_ms(&mut self, id: Option<ReportId>) -> Option<u32> {
        info!("Get idle rate for {:?}", id);
        None
    }
}

struct MyDeviceHandler {
    configured: AtomicBool,
}

impl MyDeviceHandler {
    fn new() -> Self {
        MyDeviceHandler {
            configured: AtomicBool::new(false),
        }
    }
}

impl Handler for MyDeviceHandler {
    fn enabled(&mut self, enabled: bool) {
        self.configured.store(false, Ordering::Relaxed);
        if enabled {
            info!("Device enabled");
        } else {
            info!("Device disabled");
        }
    }

    fn reset(&mut self) {
        self.configured.store(false, Ordering::Relaxed);
        info!("Bus reset, the Vbus current limit is 100mA");
    }

    fn addressed(&mut self, addr: u8) {
        self.configured.store(false, Ordering::Relaxed);
        info!("USB address set to: {}", addr);
    }

    fn configured(&mut self, configured: bool) {
        self.configured.store(configured, Ordering::Relaxed);
        if configured {
            info!(
                "Device configured, it may now draw up to the configured current limit from Vbus."
            )
        } else {
            info!("Device is no longer configured, the Vbus current limit is 100mA.");
        }
    }
}

// send text as KeyPress and KeyRelease events
async fn send_text(
    text: &str,
    sender: &embassy_sync::channel::Sender<'static, CriticalSectionRawMutex, KeyEvent, 4>,
) {
    for c in text.chars() {
        let keycode = char_to_keycode(c);
        if let Some((code, modifier)) = keycode {
            sender.send(KeyEvent::Pressed((Some(code), modifier))).await;
            Timer::after(Duration::from_millis(50)).await;
            sender
                .send(KeyEvent::Released((Some(code), modifier)))
                .await;
            Timer::after(Duration::from_millis(50)).await;
        }
    }
}

fn char_to_keycode(c: char) -> Option<(u8, Option<u8>)> {
    match c {
        'a'..='z' => Some(((c as u8 - b'a') + 0x04, None)),
        'A'..='Z' => Some(((c as u8 - b'A') + 0x04, Some(0x02))), // left shift
        '1'..='9' => Some(((c as u8 - b'1') + 0x1E, None)),
        '0' => Some((0x27, None)),
        ' ' => Some((0x2C, None)),
        '\n' => Some((0x28, None)),
        ':' => Some((0x33, Some(0x02))), // Shift + semicolon
        _ => None,
    }
}
