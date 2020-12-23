module asteroids.storage.unique;

import std.typecons;
import asteroids.entity;
import asteroids.storage.type;

/// Storage of component that can contain maximum 1 instance of component. Only
/// single entity can have the component.
class UniqueStorage(T): IStorage!T {
  Nullable!T unique;
  Entity owner = global;

  /// Stored element type
  alias Elem = T;

  /// Initialize storage
  this() {
    unique.nullify();
    owner = global;
  }

  /// Insert component for entity
  void insert(Entity e, T c) {
    unique = c;
    owner = e;
  }

  /// Get component for the entity
  T get(Entity e) {
    assert(e == owner, "Getting component for entity that doesn't have unique component, asked: "
      ~ e.stringof ~ ", but unique owned by: " ~ owner.stringof);
    assert(!unique.isNull, "Getting not existing unique component!");
    return unique.get;
  }

  /// Get mutable component for the entity
  ref T get_ref(Entity e) {
    assert(e == owner, "Getting component for entity that doesn't have unique component, asked: "
      ~ e.stringof ~ ", but unique owned by: " ~ owner.stringof);
    assert(!unique.isNull, "Getting not existing unique component!");
    return unique.get;
  }
}
