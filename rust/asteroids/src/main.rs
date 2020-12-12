#![feature(default_free_fn)]

extern crate sdl2;
extern crate specs;
extern crate glam;
#[macro_use]
extern crate specs_derive;
extern crate shrev;
extern crate rand;

use asteroids::components::player::*;
use asteroids::components::size::*;
use asteroids::components::time::DeltaTime;
use asteroids::render::render_world;
use asteroids::systems::init_systems;
use asteroids::systems::audio::*;
use asteroids::world::init_world;
use sdl2::event::Event;
use sdl2::event::WindowEvent::SizeChanged;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::mixer::{InitFlag, AUDIO_S16LSB, DEFAULT_CHANNELS};
use sdl2::pixels::Color;
use sdl2::render::WindowCanvas;
use specs::Dispatcher;
use specs::World;
use specs::WorldExt;
use specs::System;
use std::collections::HashSet;
use std::time::Instant;

mod asteroids;

/// Itialize graphic pipeline, event processing and game logic.
fn initialize<'a, 'b>() -> Result<(WindowCanvas, EventPump, World, Dispatcher<'a, 'b>), String> {
    // Initialize libraries
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let _audio = sdl_context.audio()?;

    let _mixer_context =
        sdl2::mixer::init(InitFlag::MP3 | InitFlag::FLAC | InitFlag::MOD | InitFlag::OGG)?;

    // Number of mixing channels available for sound effect `Chunk`s to play
    // simultaneously.
    sdl2::mixer::allocate_channels(20);

    // Initialize world
    let mut world = init_world();
    let dispatcher : Dispatcher<'a, 'b> = init_systems(&mut world)?;

    // Initialize window systems
    let window = video_subsystem.window("Asteroids game", DEF_WORLD_WIDTH, DEF_WORLD_HEIGHT)
        .position_centered()
        .build()
        .expect("could not initialize video subsystem");

    let mut canvas = window.into_canvas().build().expect("could not make a canvas");

    // Render first empty frame
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let event_pump = sdl_context.event_pump()?;

    Ok((canvas, event_pump, world, dispatcher))
}

/// Main game loop where game runs 99% of time. It does processing of input events, rendering and execution of game logic.
fn game_loop<'a, 'b>(mut canvas: WindowCanvas, mut event_pump: EventPump, mut world: World, mut dispatcher: Dispatcher<'a, 'b>) -> Result<(), String>  {
    // Initialize audio here, as audio device will be deallocated if we initialize it outside the function.
    let frequency = 44_100;
    let format = AUDIO_S16LSB; // signed 16 bit samples, in little-endian byte order
    let channels = DEFAULT_CHANNELS; // Stereo
    let chunk_size = 1_024;
    sdl2::mixer::open_audio(frequency, format, channels, chunk_size)?;

    // Audio system is somewhat special, it should be thread local and must be initialized after
    // audio device is opened.
    let mut audio_sys = SysAudio::new(&mut world)?;
    let mut i = 0; // counter for fps reporting
    'running: loop {
        // Start frame, save current time for FPS counter and physics delta time
        let frame_begin = Instant::now();

        // Process all events, if function returns true
        if process_events(&mut world, &mut event_pump)? {
            break 'running Ok(())
        }

        // Execute all game logic
        dispatcher.dispatch(&world);
        world.maintain();

        // Play sounds
        audio_sys.run(world.system_data());

        // Render all
        render_world(&mut canvas, world.system_data())?;

        // Write delta time and report FPS
        let mut delta = world.write_resource::<DeltaTime>();
        *delta = calc_fps(frame_begin, &mut i);
    }
}

/// Helper to report FPS counter and calculation of delta time for physics. The second counter
/// tracks count of frames to report about FPS not every frame.
fn calc_fps(frame_begin: Instant, i: &mut i32) -> DeltaTime {
    let frame_end = Instant::now();
    let dt = frame_end.duration_since(frame_begin);
    let fps = 1.0 / dt.as_secs_f64();
    if *i == 0 {
        println!("{} FPS", fps);
    }
    *i = (*i + 1) % 2000;
    DeltaTime(dt)
}

/// Iterate over all occured events from system and user and feed them inside game logic
fn process_events(world: &mut World, event_pump: &mut EventPump) -> Result<bool, String> {
    let mut wsize = world.write_resource::<WorldSize>();
    let mut pinput = world.write_resource::<HashSet<PlayerInput>>();

    for event in event_pump.poll_iter() {
        match event {
            Event::Quit {..} => return Ok(true),
            Event::KeyDown { keycode: Some(kc), .. } => match kc {
                Keycode::Escape => return Ok(true),
                Keycode::Up => { pinput.insert(PlayerInput::Thrust); },
                Keycode::Left => { pinput.insert(PlayerInput::RotateLeft); },
                Keycode::Right => { pinput.insert(PlayerInput::RotateRight); },
                Keycode::Space => { pinput.insert(PlayerInput::Fire); },
                _ => ()
            },
            Event::KeyUp { keycode: Some(kc), .. } => match kc {
                Keycode::Up => { pinput.remove(&PlayerInput::Thrust); },
                Keycode::Left => { pinput.remove(&PlayerInput::RotateLeft); },
                Keycode::Right => { pinput.remove(&PlayerInput::RotateRight); },
                Keycode::Space => { pinput.remove(&PlayerInput::Fire); },
                _ => ()
            },
            Event::Window { win_event: SizeChanged(w, h), .. } => {
                *wsize = WorldSize(w as u32, h as u32);
            },
            _ => ()
        }
    }
    Ok(false)
}

pub fn main() -> Result<(), String>  {
    let (canvas, event_pump, world, dispatcher) = initialize()?;
    game_loop(canvas, event_pump, world, dispatcher)
}
