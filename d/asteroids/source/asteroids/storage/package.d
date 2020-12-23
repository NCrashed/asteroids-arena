module asteroids.storage;

public import asteroids.storage.entity;
public import asteroids.storage.global;
public import asteroids.storage.type;
public import asteroids.storage.unique;
public import asteroids.storage.vector;

import asteroids.component;

/// Special kind of tuple with all storages for specified components that you
/// can access by their names.
struct Storages(U...) {
  mixin Components!U.Storages;
}
