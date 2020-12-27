module asteroids.component.player;

public import asteroids.component.primitive;

import decs.storage.unique;
import std.math;
import std.meta;

/// Player component that is unique per world
struct Player {
  /// Whether player ship is accelerating at the moment
  bool thrust = false;
  /// Counts until moment when player can emit a new bullet
  float fireTimer = fireCooldown;

  /// Name of player component
  enum name = "player";
  /// Storage type for player. We have only one entity that has the component.
  alias Storage = UniqueStorage!Player;

  /// Mass of player in kgs
  enum float mass = 100000;
  /// Collision radius of player in meters
  enum float radius = 15;

  /// Visual size of ship in pixels
  enum v2f renderSize = v2f(30, 25);

  /// Rotation speed of ship in radians per second
  enum float rotationSpeed = PI;
  /// Player thrust in newtons
  enum float thrustForce = 20000000;
  /// Time between two bullets
  enum float fireCooldown = 0.3;
}

/// Shorthand for enumeration of components that player entity has
alias PlayerComponents = AliasSeq!(
  Player,
  Position,
  Velocity,
  Rotation,
  Radius,
  Mass
);
