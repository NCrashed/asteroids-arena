use glam::Vec2;
use shrev::EventChannel;
use specs::prelude::*;
use std::fmt::Display;

use super::super::components::bullet::*;
use super::super::components::pos::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that spawns bullets and destroys them when they live timer is over.
pub struct SysBullet {
    reader: ReaderId<BulletSpawn>
}

impl SysBullet {
    pub fn new(world: &mut World) -> Self {
        <Self as System<'_>>::SystemData::setup(world);
        let reader_id = world.fetch_mut::<EventChannel<BulletSpawn>>().register_reader();
        Self { reader: reader_id }
    }
}

impl<'a> System<'a> for SysBullet {
    type SystemData = (
        Read<'a, EventChannel<BulletSpawn>>,
        Read<'a, DeltaTime>,
        Entities<'a>,
        WriteStorage<'a, Bullet>,
        WriteStorage<'a, Pos>,
        WriteStorage<'a, Vel>);

    fn run(&mut self, (chan, delta, entities, mut bullet, mut pos, mut vel): Self::SystemData) {
        let dt = delta.0.as_secs_f32();

        for event in chan.read(&mut self.reader) {
            let res = spawn_bullet(&entities, &mut bullet, &mut pos, &mut vel, event);
            match res {
                Err(msg) => println!("Failed to spawn bullet: {}", msg),
                _ => ()
            }
        }

        for bullet in (&mut bullet).join() {

        }
    }
}

/// Helper to transform results from specific type of errors to string one without data
trait Stringify {
    fn stringify(self) -> Result<(), String>;
}

impl<T, E:Display> Stringify for Result<T, E> {
    fn stringify(self) -> Result<(), String> {
        self.map_err(|x| format!("{}", x)).map(|_| ())
    }
}

fn spawn_bullet<'a>(
    entities: &Entities<'a>,
    bullet: &mut WriteStorage<'a, Bullet>,
    pos: &mut WriteStorage<'a, Pos>,
    vel: &mut WriteStorage<'a, Vel>,
    ev: &BulletSpawn
    ) -> Result<(), String>
{
    let e = entities.create();
    bullet.insert(e, Bullet(BULLET_LIFE_TIME)).stringify()?;
    pos.insert(e, Pos(ev.pos)).stringify()?;
    vel.insert(e, Vel(ev.vel)).stringify()
}
