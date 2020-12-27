module asteroids.system.bullet;

import asteroids.component;

/// Create bullet at give point and direction
Entity spawnBullet(Storages!(Entities, BulletComponents) storages, v2f pos, float angle, v2f vel) {
  immutable e = storages.entities.create();

  storages.set!BulletComponents(e,
    Bullet.init,
    Position(pos),
    Velocity(vel + v2f.fromAngle(angle) * Bullet.speed),
    );
  return e;
}

/// Update life timers for bullet and remove them if needed
void updateBullet(Storages!(Entities, Bullet) storages, size_t i, Entity e, float dt) {
  if(storages.entities.aliveHas!Bullet(i)) {
    storages.bullet.modify(e, (a) { a.time -= dt; return a; });
    if(storages.bullet.get(e).time <= 0) {
      storages.entities.remove(e);
    }
  }
}
