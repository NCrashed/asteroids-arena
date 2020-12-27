module decs.storage.global;

import decs.entity;

/// Storage of single component
class GlobalStorage(T) {
  T global;

  /// Stored element type
  alias Elem = T;

  final:

  /// Insert component for entity
  void insert(Entity e, T c) {
    global = c;
  }

  /// Get component for the entity
  T get(Entity e) {
    return global;
  }

  /// Get mutable component for the entity
  T* getRef(Entity e) {
    return &global;
  }

  /// Apply given function to component of the given entity.
  void modify(Entity e, T delegate(T) fun)
  {
    global = fun(global);
  }
}
