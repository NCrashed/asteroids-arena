module asteroids.component;

public import asteroids.component.asteroid;
public import asteroids.component.audio;
public import asteroids.component.bullet;
public import asteroids.component.delta;
public import asteroids.component.player;
public import asteroids.component.primitive;
public import asteroids.component.rand;
public import asteroids.component.size;
public import asteroids.component.meta;
public import asteroids.entity;

import asteroids.storage.entity;
import std.meta;

/// Here we define all components that are used
alias AllComponents = AliasSeq!(
  Entities,
  WorldSize,
  Rng,
  DeltaTime,
  Audio,
  Position,
  Velocity,
  Rotation,
  Radius,
  Mass,
  Player,
  Asteroid,
  Bullet,
  );

/// Short name for subset of components for systems
alias Storages(T) = Components!AllComponents.Storages(T);
