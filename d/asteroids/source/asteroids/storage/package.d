module asteroids.storage;

public import asteroids.storage.entity;
public import asteroids.storage.global;
public import asteroids.storage.type;
public import asteroids.storage.unique;
public import asteroids.storage.vector;

import asteroids.component;
import asteroids.entity;
import std.meta;

/// Special kind of tuple with all storages for specified components that you
/// can access by their names.
struct Storages(U...) {
  mixin Components!U.Storages;

  /// Set components $(B C) for given entity and registry them in entities storage
  void set(C...)(Entity e, C cs)
    // if (hasComponent!(Entities, U))
  {
    static assert(hasComponent!(Entities, U), "Storages.set require Entities component within components: " ~ U.stringof);
    static foreach(c; cs) {
      mixin(c.name).insert(e, c);
    }
    entities.addComponents!C(e);
  }
}

/// Helper that defines that component C is across components T
template hasComponent(C, T...) {
  enum hasComponent = staticIndexOf!(C, T) >= 0;
}
