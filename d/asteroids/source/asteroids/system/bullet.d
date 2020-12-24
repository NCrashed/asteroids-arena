module asteroids.system.bullet;

import asteroids.component;
import asteroids.storage;

/// Create bullet at give point and direction
Entity spawnBullet(Storages!(Entities, BulletComponents) storages, v2f pos, float angle) {
  immutable e = storages.entities.create();

  storages.set!BulletComponents(e,
    Bullet.init,
    Position(pos),
    Velocity(v2f.fromAngle(angle) * Bullet.speed),
    );
  return e;
}
