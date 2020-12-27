module asteroids.component.asteroid;

public import asteroids.component.primitive;

import decs.storage.vector;
import std.math;
import std.meta;
import std.typecons;

/// Marks that the given entity is asteroid
struct Asteroid {
  /// Amount of edges for rendering
  int edges;

  /// Name of asteroid component. Field in storages will have this name.
  enum name = "asteroid";
  /// Storage type for asteroid. We store them in array.
  alias Storage = VecStorage!Asteroid;

  /// Defines minimum and maximum amound of edges asteroid can have
  enum edgesRange = tuple(8, 20);
  /// Defines minimum and maximum radius that asteroid can have
  enum sizeRange = tuple(10, 130);
  /// Defines minimum and maximum velocity that asteroid can have
  enum velocityRange = tuple(-100, 100);
  /// Density of asteroid kg per px*px
  enum density = 1.0;
  /// Amount of asteroids at generation time
  enum amount = 20;
}

/// Shorthand for enumeration of components that asteroid entity has
alias AsteroidComponents = AliasSeq!(
  Asteroid,
  Position,
  Velocity,
  Rotation,
  Radius,
  Mass
);
