module asteroids.system.physics;

import asteroids.component;
import asteroids.system.player;
import asteroids.system.asteroid;

/// System that iterates over all alive entities and process movement and collisions
void physicsSystem(Storages!AllComponents storages, size_t i, Entity e) {
  if(storages.entities.aliveHas!(Position, Velocity)(i)) {
    movement(storages.sub!(Position, Velocity, DeltaTime), e);
    warping(storages.sub!(Position, WorldSize), e);
  }
  if(storages.entities.aliveHas!(Asteroid, Position, Radius)(i)) {
    immutable apos = storages.get!Position(e);
    immutable arad = storages.get!Radius(e);
    if(!storages.player.unique.isNull) {
      immutable pe = storages.player.owner;
      immutable ppos = storages.get!Position(pe);
      immutable prad = storages.get!Radius(pe);
      immutable r = prad + arad;
      if(apos.distSquared(ppos) <= r*r) {
        killPlayer(storages.sub!(Entities, WorldSize, PlayerComponents), pe);
        storages.audio.global.play(Sound.bang);
      }
    }
    for(size_t j=i+1; j<storages.entities.alive.length; j++) {
      if(storages.entities.aliveHas!(Bullet, Position)(j)) {
        immutable be = storages.entities.alive[j];
        checkAsteroidHit(storages.sub!(Entities, Rng, Audio, WorldSize, AsteroidComponents), e, be);
      }
    }
  } else if (storages.entities.aliveHas!(Bullet, Position)(i)) {
    for(size_t j=i+1; j<storages.entities.alive.length; j++) {
      if(storages.entities.aliveHas!(Asteroid, Position, Radius)(j)) {
        immutable ae = storages.entities.alive[j];
        checkAsteroidHit(storages.sub!(Entities, Rng, Audio, WorldSize, AsteroidComponents), ae, e);
      }
    }
  }
}

/// Check collision between asteroid and bullet
private void checkAsteroidHit(Storages!(Entities, Rng, Audio, WorldSize, AsteroidComponents) storages, Entity astEnt, Entity bullEnt) {
  immutable ac = storages.get!(Asteroid, Position, Velocity, Rotation, Radius)(astEnt);
  immutable bpos = storages.get!Position(bullEnt);
  immutable r = ac.radius + Bullet.radius;
  if(ac.position.distSquared(bpos) <= r*r) {
    storages.entities.remove(astEnt);
    storages.entities.remove(bullEnt);
    spawnShard(storages.sub!(Entities, Rng, AsteroidComponents), ac.asteroid, ac.position, ac.velocity, ac.rotation, ac.radius);
    spawnShard(storages.sub!(Entities, Rng, AsteroidComponents), ac.asteroid, ac.position, ac.velocity, ac.rotation, ac.radius);
    storages.audio.global.play(Sound.bang);
  }
}

/// System that applies position and velocity to given entity
private void movement(Storages!(Position, Velocity, DeltaTime) storages, Entity e) {
  immutable vel = storages.velocity.get(e);
  immutable dt = storages.deltaTime.global.dt;
  storages.position.modify(e, p => p + vel * dt);
}

/// System that warps space
private void warping(Storages!(Position, WorldSize) storages, Entity e) {
  immutable ws = storages.worldSize.global;
  auto pos = storages.position.getRef(e);

  if (pos.x < 0) pos.x += ws.width;
  else if (pos.x > ws.width) pos.x -= ws.width;
  if (pos.y < 0) pos.y += ws.height;
  else if (pos.y > ws.height) pos.y -= ws.height;
}
