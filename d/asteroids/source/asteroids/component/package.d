module asteroids.component;

public import asteroids.component.asteroid;
public import asteroids.component.audio;
public import asteroids.component.bullet;
public import asteroids.component.delta;
public import asteroids.component.player;
public import asteroids.component.primitive;
public import asteroids.component.rand;
public import asteroids.component.size;

public import decs;
import std.meta;

// Also injects AllComponents alias to the list
mixin DeclareComponents!(
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
