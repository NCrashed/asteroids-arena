module asteroids.storage.entity;

import asteroids.component;
import asteroids.entity;
import std.algorithm;
import std.container.array;

/// Special storage of world that tracks alive entities and their components.
///
/// Template arguments are all supported entities.
struct Entities(T...) {
  /// Next free entity ID
  size_t entityCounter = 0;
  /// Collection of alive entities
  Array!Entity alive;
  /// Components tags for iteration
  Array!ComponentTag tags;

  /// Shorthand for components utilities
  private alias CS = Components!T;

  /// Create and registry new entity
  Entity create() {
    immutable e = entityCounter;
    entityCounter += 1;
    alive.insertBack(e);
    tags.insertBack(none);
    return e;
  }

  /// Remove entity from alive entities. Marks them as
  /// dead. They are deleted at end of frame with call
  /// to maintain method.
  void destory(Entity e) {
    deleted.insertBack(e);
  }

  /// Remove entity from alive entity. Pefrom destruction
  /// at the current moment, so alive entities change their
  /// ids. Don't use it when iterating over alive entities.
  void destroyNow(Entity e) {
    foreach(i, ae; alive[]) {
      if (ae == e) {
        alive.swapRemove(i);
        tags.swapRemove(i);
        return;
      }
    }
  }

  /// Finalize removal of entities that was lazy deleted.
  /// After the operation indecies of alive entities are
  /// changed. The operation is designed to be called at
  /// end of frame.
  void maintain() {
    foreach(e; deleted[]) {
      destroyNow(e);
    }
    deleted.clear();
  }
}

/// Remove an element from array and swap last element to the hole
private void swapRemove(T)(Array!T arr, size_t i) {
  arr[i] = arr[$-1];
  arr.removeBack();
}
unittest {
  auto a = Array!int(0, 2, 3, 4, 5);
  a.swapRemove(0);
  assert(a[].equal([5, 2, 3, 4]));
  a.swapRemove(3);
  assert(a[].equal([5, 2, 3]));
  a.swapRemove(1);
  assert(a[].equal([5, 3]));
  a.swapRemove(1);
  assert(a[].equal([5]));
  a.swapRemove(0);
  assert(a.length == 0);
}
