module asteroids.system.player;

import asteroids.component;
import std.stdio;

Entity spawnPlayer(Storages!(Entities, WorldSize, PlayerComponents) storages) {
  immutable e = storages.entities.create();
  immutable x = cast(float)storages.worldSize.global.width * 0.5;
  immutable y = cast(float)storages.worldSize.global.height * 0.5;

  storages.set!PlayerComponents(e,
    Player.init,
    Position(v2f(x, y)),
    Velocity(v2f(0, 0)),
    Rotation(0),
    Radius(Player.radius),
    Mass(Player.mass),
    );
  return e;
}

void killPlayer(Storages!(Entities, WorldSize, PlayerComponents) storages, Entity e) {
  storages.entities.remove(e);
  spawnPlayer(storages);
}

void updatePlayer(Player.Storage player, float dt) {
  if (!player.unique.isNull) {
    if (player.unique.get.fireTimer > 0) {
      player.unique.get.fireTimer -= dt;
    }
  }
}
