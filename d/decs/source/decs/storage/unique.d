module decs.storage.unique;

import decs.entity;
import std.typecons;

/// Storage of component that can contain maximum 1 instance of component. Only
/// single entity can have the component.
class UniqueStorage(T) {
  Nullable!T unique;
  Entity owner = global;

  /// Stored element type
  alias Elem = T;

  /// Initialize storage
  this() {
    unique.nullify();
    owner = global;
  }

  final:

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
  T* getRef(Entity e) {
    assert(e == owner, "Getting component for entity that doesn't have unique component, asked: "
      ~ e.stringof ~ ", but unique owned by: " ~ owner.stringof);
    assert(!unique.isNull, "Getting not existing unique component!");
    return &unique.get();
  }

  /// Apply given function to component of the given entity.
  void modify(Entity e, T delegate(T) fun)
  {
    assert(e == owner, "Modifying component for entity that doesn't have unique component, asked: "
      ~ e.stringof ~ ", but unique owned by: " ~ owner.stringof);
    assert(!unique.isNull, "Modifying unique component that doesn't exists!");
    T res = fun(unique.get);
    unique = res;
  }
}
