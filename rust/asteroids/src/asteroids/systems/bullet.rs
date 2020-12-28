use crossbeam::channel::*;
use legion::*;
use legion::systems::*;
use legion::world::*;

use super::super::components::bullet::*;
use super::super::components::pos::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that spawns bullets and destroys them when they live timer is over.
#[system]
#[write_component(Bullet)]
pub fn bullet(
    cmd: &mut CommandBuffer,
    world: &mut SubWorld,
    #[resource] chan: &Receiver<BulletSpawn>,
    #[resource] delta: &DeltaTime)
{
    let dt = delta.0.as_secs_f32();

    // Spawn bullets
    for event in chan.try_iter() {
        cmd.push((
            Bullet(BULLET_LIFE_TIME),
            Pos(event.pos),
            Vel(event.vel),
        ));
    }

    // Destroy bullets
    let mut query = <(&mut Bullet, Entity)>::query();
    for (bullet, e) in query.iter_mut(world) {
        bullet.0 -= dt;
        if bullet.0 <= 0.0 {
            cmd.remove(*e);
        }
    }
}
