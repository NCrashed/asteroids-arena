#![feature(default_free_fn)]

extern crate sdl2;
extern crate specs;
extern crate glam;
#[macro_use]
extern crate specs_derive;

use asteroids::components::player::*;
use asteroids::components::size::*;
use asteroids::components::time::DeltaTime;
use asteroids::render::render_world;
use asteroids::systems::init_systems;
use asteroids::world::init_world;
use sdl2::event::Event;
use sdl2::event::WindowEvent::SizeChanged;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::EventPump;
use specs::World;
use specs::WorldExt;
use std::time::Instant;

mod asteroids;

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

fn process_events(world: &mut World, event_pump: &mut EventPump) -> Result<bool, String> {
    let mut wsize = world.write_resource::<WorldSize>();
    let mut pinput = world.write_resource::<Vec<PlayerInput>>();

    for event in event_pump.poll_iter() {
        match event {
            Event::Quit {..} => return Ok(true),
            Event::KeyDown { keycode: Some(kc), .. } => match kc {
                Keycode::Escape => return Ok(true),
                Keycode::Up => pinput.push(PlayerInput::Thrust),
                Keycode::Left => pinput.push(PlayerInput::RotateLeft),
                Keycode::Right => pinput.push(PlayerInput::RotateRight),
                Keycode::Space => pinput.push(PlayerInput::Fire),
                _ => ()
            },
            Event::Window { win_event: SizeChanged(w, h), .. } => {
                *wsize = WorldSize(w as u32, h as u32);
            },
            _ => {}
        }
    }
    Ok(false)
}

pub fn main() -> Result<(), String>  {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let mut world = init_world();
    let mut dispatcher = init_systems(&mut world);

    let window = video_subsystem.window("Asteroids game", DEF_WORLD_WIDTH, DEF_WORLD_HEIGHT)
        .position_centered()
        .build()
        .expect("could not initialize video subsystem");

    let mut canvas = window.into_canvas().build().expect("could not make a canvas");

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump()?;
    let mut i = 0;
    'running: loop {
        let frame_begin = Instant::now();
        if process_events(&mut world, &mut event_pump)? {
            break 'running Ok(())
        }

        render_world(&mut canvas, world.system_data())?;

        dispatcher.dispatch(&world);
        world.maintain();

        let mut delta = world.write_resource::<DeltaTime>();
        *delta = calc_fps(frame_begin, &mut i);
    }
}
