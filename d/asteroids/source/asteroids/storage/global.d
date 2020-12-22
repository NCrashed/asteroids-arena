module asteroids.storage.global;

/// Storage of single component
struct GlobalStorage(T) {
  T global;

  /// Stored element type
  alias Elem = T;
}
