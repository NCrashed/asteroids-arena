module asteroids.system.physics;

import asteroids.component;
import asteroids.storage;

/// System that iterates over all alive entities and process movement and collisions
void physicsSystem(Storages!AllComponents storages) {
  foreach(i, e; storages.entities) {
    if(storages.entities.aliveHas!(Position, Velocity)(i)) {
      movement(storages.sub!(Position, Velocity, DeltaTime), e);
    }
  }
}

/// System that applies position and velocity to given entity
private void movement(Storages!(Position, Velocity, DeltaTime) storages, Entity e) {
    immutable vel = storages.velocity.get(e);
    immutable dt = storages.deltaTime.global.dt;
    storages.position.modify(e, p => p + vel * dt);
}
