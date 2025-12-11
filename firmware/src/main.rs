#![no_std]
#![no_main]

mod fmt;

use core::sync::atomic::{AtomicU8, Ordering};

use defmt::Format;
use embassy_futures::join::join;
use embassy_sync::{blocking_mutex::raw::CriticalSectionRawMutex, channel::Channel};
use embassy_usb::{
    class::hid::{self, HidReaderWriter, HidWriter, State},
    Builder,
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
    exti::{self, ExtiInput},
    gpio::{Input, Level, Output, Pull, Speed},
    pac::iwdg::vals::Key,
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
            freq: Hertz(16_000_000),
            mode: HseMode::Oscillator,
        });

        config.rcc.pll = Some(Pll {
            src: PllSource::HSE,
            prediv: PllPreDiv::DIV2,
            mul: PllMul::MUL9,
        });
        config.rcc.sys = Sysclk::PLL1_P;
        config.rcc.ahb_pre = AHBPrescaler::DIV1;
        config.rcc.apb1_pre = APBPrescaler::DIV2;
        config.rcc.apb2_pre = APBPrescaler::DIV1;
    }
    let p = embassy_stm32::init(config);

    info!("Hello, World!");

    let driver = Driver::new(p.USB, Irqs, p.PA12, p.PA11);

    // Create embassy-usb config
    let mut config = embassy_usb::Config::new(0xc0de, 0xcafe);
    config.manufacturer = Some("3nt3");
    config.product = Some("Transfem Keyboard :3");

    let mut config_descriptor = [0; 256];
    let mut bos_descriptor = [0; 256];
    let mut control_buf = [0; 7];

    let mut state = State::new();

    let mut builder = Builder::new(
        driver,
        config,
        &mut config_descriptor,
        &mut bos_descriptor,
        &mut [],
        &mut control_buf,
    );

    let hid_config = hid::Config {
        report_descriptor: KeyboardReport::desc(),
        request_handler: None,
        poll_ms: 60,
        max_packet_size: 8,
    };

    let mut writer = HidWriter::<_, 8>::new(&mut builder, &mut state, hid_config);

    let mut usb = builder.build();

    let usb_fut = usb.run();

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
                KeyEvent::Pressed((Some(code), _)) => {
                    // Insert keycode if there's space and not repeated
                    if !keys.contains(&code) {
                        if let Some(slot) = keys.iter_mut().find(|s| **s == 0) {
                            *slot = code;
                        }
                    }
                }
                KeyEvent::Pressed((_, Some(new_modifier))) => {
                    // Insert keycode if there's space and not repeated
                    modifier |= new_modifier;
                }
                KeyEvent::Released((Some(code), _)) => {
                    // Remove keycode from list
                    for k in &mut keys {
                        if *k == code {
                            *k = 0;
                        }
                    }
                }
                KeyEvent::Released((_, Some(new_modifier))) => {
                    modifier &= !new_modifier;
                }
                _ => { /* NOP */ }
            }

            // Decide boot vs report protocol
            if HID_PROTOCOL_MODE.load(Ordering::Relaxed) == HidProtocolMode::Boot as u8 {
                let mut boot: [u8; 8] = [0; 8];
                boot[2..8].copy_from_slice(&keys);
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

    // future that spams 'a'
    let a_fut = async {
        loop {
            // FIXME: just for debuggin

            info!("Sending 'a' key");
            sender.send(KeyEvent::Pressed((Some(0x04), None))).await; // 'a' key

            Timer::after(Duration::from_millis(500)).await;
        }
    };

    join(usb_fut, join(writer_fut, a_fut)).await;
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
