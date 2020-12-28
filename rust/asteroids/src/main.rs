#![feature(default_free_fn)]

extern crate crossbeam;
extern crate glam;
extern crate legion;
extern crate rand;
extern crate sdl2;

use asteroids::components::player::*;
use asteroids::components::size::*;
use asteroids::components::time::DeltaTime;
use asteroids::systems::*;
use asteroids::world::init_world;
use legion::*;
use sdl2::event::Event;
use sdl2::event::WindowEvent::SizeChanged;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::mixer::{InitFlag, AUDIO_S16LSB, DEFAULT_CHANNELS};
use sdl2::pixels::Color;
use sdl2::render::WindowCanvas;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;
use std::time::Instant;

mod asteroids;

/// Itialize graphic pipeline, event processing and game logic.
fn initialize<'a, 'b>() -> Result<(WindowCanvas, EventPump), String> {
    // Initialize libraries
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let _audio = sdl_context.audio()?;

    let _mixer_context =
        sdl2::mixer::init(InitFlag::MP3 | InitFlag::FLAC | InitFlag::MOD | InitFlag::OGG)?;

    // Number of mixing channels available for sound effect `Chunk`s to play
    // simultaneously.
    sdl2::mixer::allocate_channels(20);

    // Initialize window systems
    let window = video_subsystem
        .window("Asteroids game", DEF_WORLD_WIDTH, DEF_WORLD_HEIGHT)
        .position_centered()
        .build()
        .expect("could not initialize video subsystem");

    let mut canvas = window
        .into_canvas()
        .build()
        .expect("could not make a canvas");

    // Render first empty frame
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let event_pump = sdl_context.event_pump()?;

    Ok((canvas, event_pump))
}

/// Main game loop where game runs 99% of time. It does processing of input events, rendering and execution of game logic.
fn game_loop<'a, 'b>(
    canvas: WindowCanvas,
    mut event_pump: EventPump,
) -> Result<(), String> {
    // Initialize audio here, as audio device will be deallocated if we initialize it outside the function.
    let frequency = 44_100;
    let format = AUDIO_S16LSB; // signed 16 bit samples, in little-endian byte order
    let channels = DEFAULT_CHANNELS; // Stereo
    let chunk_size = 1_024;
    sdl2::mixer::open_audio(frequency, format, channels, chunk_size)?;

    // Initialize world
    let (mut world, mut resources) = init_world();
    let mut schedule: Schedule = init_systems()?;

    // Set canvas here as we move it to resources. Rendering system is embedded as thread local system.
    resources.insert(canvas);

    let mut i = 0; // counter for fps reporting
    let mut file = File::create("fps.out").map_err(|x| format!("{}", x))?; // fps reporting file
    'running: loop {
        // Start frame, save current time for FPS counter and physics delta time
        let frame_begin = Instant::now();

        // Process all events, if function returns true
        if process_events(&mut resources, &mut event_pump)? {
            break 'running Ok(());
        }

        // Execute all game logic
        schedule.execute(&mut world, &mut resources);

        // Write delta time and report FPS
        resources.insert(calc_fps(frame_begin, &mut i, &mut file)?);
    }
}

/// Helper to report FPS counter and calculation of delta time for physics. The second counter
/// tracks count of frames to report about FPS not every frame.
fn calc_fps(frame_begin: Instant, i: &mut i32, file: &mut File) -> Result<DeltaTime, String> {
    let frame_end = Instant::now();
    let dt = frame_end.duration_since(frame_begin);
    let fps = 1.0 / dt.as_secs_f64();
    if *i % 1000 == 0 {
        println!("{} FPS", fps);
        write!(file, "{},{}\n", i, fps).map_err(|x| format!("{}", x))?;
    }
    *i = *i + 1;
    Ok(DeltaTime(dt))
}

/// Iterate over all occured events from system and user and feed them inside game logic
fn process_events(resources: &mut Resources, event_pump: &mut EventPump) -> Result<bool, String> {
    let mut wsize = resources.get_mut::<WorldSize>().unwrap();
    let mut pinput = resources.get_mut::<HashSet<PlayerInput>>().unwrap();

    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } => return Ok(true),
            Event::KeyDown {
                keycode: Some(kc), ..
            } => match kc {
                Keycode::Escape => return Ok(true),
                Keycode::Up => {
                    pinput.insert(PlayerInput::Thrust);
                }
                Keycode::Left => {
                    pinput.insert(PlayerInput::RotateLeft);
                }
                Keycode::Right => {
                    pinput.insert(PlayerInput::RotateRight);
                }
                Keycode::Space => {
                    pinput.insert(PlayerInput::Fire);
                }
                _ => (),
            },
            Event::KeyUp {
                keycode: Some(kc), ..
            } => match kc {
                Keycode::Up => {
                    pinput.remove(&PlayerInput::Thrust);
                }
                Keycode::Left => {
                    pinput.remove(&PlayerInput::RotateLeft);
                }
                Keycode::Right => {
                    pinput.remove(&PlayerInput::RotateRight);
                }
                Keycode::Space => {
                    pinput.remove(&PlayerInput::Fire);
                }
                _ => (),
            },
            Event::Window {
                win_event: SizeChanged(w, h),
                ..
            } => {
                *wsize = WorldSize(w as u32, h as u32);
            }
            _ => (),
        }
    }
    Ok(false)
}

pub fn main() -> Result<(), String> {
    let (canvas, event_pump) = initialize()?;
    game_loop(canvas, event_pump)
}
