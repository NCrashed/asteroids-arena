module asteroids.component.rand;

import decs.storage.global;
import std.random;

/// World wide random number generator
struct Rng {
  Random value;
  alias value this;

  /// Name of component
  enum name = "rng";
  /// We store size as global value
  alias Storage = GlobalStorage!Rng;
}
