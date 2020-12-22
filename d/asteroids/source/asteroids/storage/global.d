module asteroids.storage.global;

import asteroids.entity;
import asteroids.storage.type;

/// Storage of single component
class GlobalStorage(T): IStorage!T {
  T global;

  /// Stored element type
  alias Elem = T;

  /// Insert component for entity
  void insert(Entity e, T c) {
    global = c;
  }

  /// Get component for the entity
  T get(Entity e) {
    return global;
  }

  /// Get mutable component for the entity
  ref T get_ref(Entity e) {
    return global;
  }
}
