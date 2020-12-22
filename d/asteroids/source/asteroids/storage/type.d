module asteroids.storage.type;

import asteroids.entity;

/// Any storage should support these operations.
interface IStorage(T) {
  /// Insert component for entity
  void insert(Entity e, T c);
  /// Get component for the entity
  T get(Entity e);
  /// Get mutable component for the entity
  ref T get_ref(Entity e);
}
