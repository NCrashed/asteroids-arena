module asteroids.storage.vector;

import asteroids.entity;
import asteroids.storage.type;
import std.container.array;
import std.range;

/// Storage of components that is based on growing array
class VecStorage(T) : IStorage!T {
  Array!T items;

  /// Stored element type
  alias Elem = T;

  /// Initialize storage
  this() {
    items = Array!T();
  }

  /// Insert component for entity
  void insert(Entity e, T c) {
    if (items.length <= e) {
      immutable n = e - items.length + 1;
      items.reserve(n);
      items.insertBack(T.init.repeat().take(n-1));
    }
    items[e] = c;
  }

  /// Get component for the entity
  T get(Entity e) {
    assert(e < items.length, "Getting component for entity outside of range, asked: "
      ~ e.stringof ~ ", but has only: " ~ items.length.stringof);
    return items[e];
  }

  /// Get mutable component for the entity
  ref T get_ref(Entity e) {
    assert(e < items.length, "Getting component for entity outside of range, asked: "
      ~ e.stringof ~ ", but has only: " ~ items.length.stringof);
    return items[e];
  }
}
