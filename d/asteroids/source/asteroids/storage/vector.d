module asteroids.storage.vector;

import std.container.array;

/// Storage of components that is based on growing array
struct VecStorage(T) {
  Array!T items;

  /// Stored element type
  alias Elem = T;
}
