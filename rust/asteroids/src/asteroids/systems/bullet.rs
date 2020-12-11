use shrev::EventChannel;
use specs::prelude::*;

use super::super::stringify::*;
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

        // Spawn bullets
        for event in chan.read(&mut self.reader) {
            let res = spawn_bullet(&entities, &mut bullet, &mut pos, &mut vel, event);
            match res {
                Err(msg) => println!("Failed to spawn bullet: {}", msg),
                _ => ()
            }
        }

        // Destroy bullets
        for (bullet, e) in (&mut bullet, &entities).join() {
            bullet.0 -= dt;
            if bullet.0 <= 0.0 {
                match entities.delete(e).stringify() {
                    Err(msg) => println!("Failed to delete bullet {:?}: {}", e, msg),
                    _ => ()
                }
            }
        }
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
