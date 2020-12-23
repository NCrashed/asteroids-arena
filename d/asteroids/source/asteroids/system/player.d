module asteroids.system.player;

import asteroids.component;
import asteroids.entity;
import asteroids.storage;

import std.stdio;

Entity spawn_player(Storages!(Entities, WorldSize, PlayerComponents) storages) {
  immutable e = storages.entities.create();
  immutable x = cast(float)storages.worldSize.global.width * 0.5;
  immutable y = cast(float)storages.worldSize.global.height * 0.5;

  with(storages) {
    player.insert(e, Player.init);
    position.insert(e, v2f(x, y));
    velocity.insert(e, v2f(0, 0));
    rotation.insert(e, 0);
    mass.insert(e, Player.mass);
    radius.insert(e, Player.radius);
    entities.addComponents!PlayerComponents(e);
  }
  return e;
}
