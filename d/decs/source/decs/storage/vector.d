module decs.storage.vector;

import decs.entity;
import std.container.array;
import std.range;

/// Storage of components that is based on growing array
class VecStorage(T) {
  Array!T items;

  /// Stored element type
  alias Elem = T;

  /// Initialize storage
  this() {
    items = Array!T();
  }

  final:

  /// Insert component for entity
  void insert(Entity e, T c) {
    if (items.length <= e) {
      immutable n = e - items.length + 1;
      items.reserve(n);
      items.insertBack(T.init.repeat().take(n));
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
  T* getRef(Entity e) {
    assert(e < items.length, "Getting component for entity outside of range, asked: "
      ~ e.stringof ~ ", but has only: " ~ items.length.stringof);
    return &items[e];
  }

  /// Apply given function to component of the given entity.
  void modify(Entity e, T delegate(T) fun)
  {
    assert(e < items.length, "Modify component for entity outside of range, asked: "
      ~ e.stringof ~ ", but has only: " ~ items.length.stringof);
    items[e] = fun(items[e]);
  }
}
