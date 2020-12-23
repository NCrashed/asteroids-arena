module asteroids.system.player;

import asteroids.component;
import asteroids.entity;
import asteroids.storage;

Entity spawn_player(Storages!(Entities, Rng, PlayerComponents) storages) {
  immutable e = storages.entities.create();

  return e;
}
