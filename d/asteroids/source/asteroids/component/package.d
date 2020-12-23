module asteroids.component;

public import asteroids.component.player;
public import asteroids.component.primitive;
public import asteroids.component.rand;
public import asteroids.component.size;
public import asteroids.component.meta;

import asteroids.storage.entity;
import std.meta;

/// Here we define all components that are used
alias AllComponents = AliasSeq!(
  Entities,
  WorldSize,
  Rng,
  Position,
  Velocity,
  Rotation,
  Radius,
  Mass,
  Player,
  );

/// Short name for subset of components for systems
alias Storages(T) = Components!AllComponents.Storages(T);
