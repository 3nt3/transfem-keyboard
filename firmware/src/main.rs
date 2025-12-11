#![no_std]
#![no_main]

mod fmt;

use embassy_sync::{blocking_mutex::raw::CriticalSectionRawMutex, channel::Channel};
use embassy_usb::{
    class::hid::{self, HidReaderWriter, State},
    Builder,
};
#[cfg(not(feature = "defmt"))]
use panic_halt as _;
use usbd_hid::descriptor::{KeyboardReport, SerializedDescriptor};
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

struct ButtonConfig<B> {
    button: B,
    keycode: u8,
}

#[derive(Debug)]
enum KeyEvent {
    Pressed(u8),
    Released(u8),
}

type KeyChannel = Channel<CriticalSectionRawMutex, KeyEvent, 4>;
static CHANNEL: KeyChannel = KeyChannel::new();

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
            button: &shift_btn,
            keycode: 0xE1,
        }, // Left shift
        ButtonConfig {
            button: &colon_btn,
            keycode: 0x33,
        }, // Colon (:) (actually semicolon)
        ButtonConfig {
            button: &three_btn,
            keycode: 0x20,
        }, // 3
    ];

    let mut channel = embassy_sync::channel::Channel::new();
    let receiver = channel.receiver();
    let mut sender = channel.sender();

    for cfg in buttons {
        let sender = sender.clone();

        spawner
            .spawn(async move {
                loop {
                    cfg.button.wait_for_rising_edge().await;
                    sender.send(KeyEvent::Pressed(cfg.keycode)).await;

                    cfg.button.wait_for_falling_edge().await;
                    sender.send(KeyEvent::Released(cfg.keycode)).await;
                }
            })
            .unwrap();
    }

    loop {}
}

#[embassy_executor::task]
async fn button_task(
    mut button: ExtiInput<'static>,
    keycode: u8,
    sender: embassy_sync::channel::Sender<'static, KeyEvent, 4>,
) -> ! {
    loop {
        button.wait_for_rising_edge().await;
        sender.send(KeyEvent::Pressed(keycode)).await;

        button.wait_for_falling_edge().await;
        sender.send(KeyEvent::Released(keycode)).await;
    }
}
