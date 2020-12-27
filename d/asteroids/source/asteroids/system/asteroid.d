module asteroids.system.asteroid;

import asteroids.component;
import std.math;
import std.typecons;
import std.random;

/// Spawn all asteroids withing world with randomized parameters
void spawnAsteroids(Storages!(Entities, Rng, WorldSize, AsteroidComponents) storages) {
  foreach(i; 0..Asteroid.amount) {
    spawnAsteroid(storages);
  }
}

/// Spawn single asteroid within world with random velocity and size
private Entity spawnAsteroid(Storages!(Entities, Rng, WorldSize, AsteroidComponents) storages) {
  auto rng = storages.rng.global;
  immutable ws = storages.worldSize.global;

  immutable e = storages.entities.create();
  immutable x = uniform(0, cast(float)ws.width, rng);
  immutable y = uniform(0, cast(float)ws.height, rng);
  immutable r = uniform(Asteroid.sizeRange[0], Asteroid.sizeRange[1], rng);
  immutable vel = v2f.uniform(Asteroid.velocityRange[0], Asteroid.velocityRange[1], rng);

  storages.set!AsteroidComponents(e,
    Asteroid(uniform(Asteroid.edgesRange[0], Asteroid.edgesRange[1], rng)),
    Position(v2f(x, y)),
    Velocity(vel),
    Rotation(uniform(0, 2*PI, rng)),
    Radius(r),
    Mass(PI * r * r * Asteroid.density),
    );
  storages.rng.global = rng;
  return e;
}

/// Spawn new asteroid shard if asteroid has enough radius
Nullable!Entity spawnShard(Storages!(Entities, Rng, AsteroidComponents) storages, Asteroid past, v2f ppos, v2f pvel, float prot, float prad) {
  immutable r = prad * 0.5;
  if (r < Asteroid.sizeRange[0]) return Nullable!Entity();

  immutable e = storages.entities.create();
  immutable vel = v2f.uniform(Asteroid.velocityRange[0], Asteroid.velocityRange[1], storages.rng.global);

  storages.set!AsteroidComponents(e,
    past,
    Position(ppos),
    Velocity(pvel + vel),
    Rotation(prot),
    Radius(r),
    Mass(PI * r * r * Asteroid.density),
    );

  return Nullable!Entity(e);
}
