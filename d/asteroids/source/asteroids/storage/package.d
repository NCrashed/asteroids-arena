module asteroids.storage;

public import asteroids.storage.entity;
public import asteroids.storage.global;
public import asteroids.storage.unique;
public import asteroids.storage.vector;

import asteroids.component;
import asteroids.entity;
import std.meta;
import std.typecons;

/// Special kind of tuple with all storages for specified components that you
/// can access by their names.
struct Storages(U...) {
  mixin Components!U.Storages;

  /// Initialize all storages. The method should be called if the storages
  /// struct is created from fresh new storages not by $(REF set) or $(REF sub).
  void init() {
    mixin(Components!U.initStorages);
  }

  /// Read single component from the storages. Throws if entity doesn't have it.
  C get(C)(Entity e)
    if(hasComponent!(C, U))
  {
    return cast(C)mixin(C.name).get(e);
  }

  /// Read tuple of components for given entity. Throws if entity doesn't have one of them.
  Tuple!C get(C...)(Entity e)
    if(C.length > 1)
  {
    static foreach(c; C) {
      static assert(hasComponent!(c, U), "Storages.get require "~c.stringof~" component within components: " ~ U.stringof);
    }

    string collect() {
      string acc = "";
      static foreach(c; C) {
        acc ~= c.name ~ ".get(e), ";
      }
      return acc;
    }

    return mixin("tuple(" ~ collect() ~")");
  }

  /// Set components $(B C) for given entity and registry them in entities storage
  void set(C...)(Entity e, C cs)
  {
    static assert(hasComponent!(Entities, U), "Storages.set require Entities component within components: " ~ U.stringof);
    static foreach(c; cs) {
      mixin(c.name).insert(e, c);
    }
    entities.addComponents!C(e);
  }

  /// Collect subset of storages from the current storage
  Storages!C sub(C...)() {
    return mixin(Components!U.collect!C);
  }
}

/// Helper that defines that component C is across components T
template hasComponent(C, T...) {
  enum hasComponent = staticIndexOf!(C, T) >= 0;
}
