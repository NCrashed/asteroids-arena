module asteroids.storage.unique;

import std.typecons;
import asteroids.entity;

/// Storage of component that can contain maximum 1 instance of component. Only
/// single entity can have the component.
struct UniqueStorage(T) {
  Nullable!T unique;
  Entity owner;

  /// Stored element type
  alias Elem = T;
}
