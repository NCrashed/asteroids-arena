module asteroids.system.player;

import asteroids.component;
import asteroids.entity;
import asteroids.storage;

import std.stdio;

Entity spawn_player(Storages!(Entities, WorldSize, PlayerComponents) storages) {
  immutable e = storages.entities.create();
  immutable x = cast(float)storages.worldSize.global.width * 0.5;
  immutable y = cast(float)storages.worldSize.global.height * 0.5;

  storages.player.insert(e, Player.init);

  return e;
}
