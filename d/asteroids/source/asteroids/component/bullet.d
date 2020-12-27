module asteroids.component.bullet;

public import asteroids.component.primitive;

import decs.storage.vector;
import std.meta;

/// Tags bullet entities and contains timer when bullet should despawn
struct Bullet {
  /// Remaining time until bullet is destroyed
  float time = lifeTime;

  /// Name of asteroid component. Field in storages will have this name.
  enum name = "bullet";
  /// Storage type for asteroid. We store them in array.
  alias Storage = VecStorage!Bullet;
  /// Bullet spawn velocity absolute value
  enum float speed = 200;
  /// Amount of seconds bullet lives after spawn
  enum float lifeTime = 3.0;
  /// Collision radius for bullet
  enum float radius = 1.0;
}

/// Shorthand for enumeration of components that asteroid entity has
alias BulletComponents = AliasSeq!(
  Bullet,
  Position,
  Velocity,
);
