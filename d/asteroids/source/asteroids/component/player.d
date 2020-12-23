module asteroids.component.player;

public import asteroids.component.primitive;

import asteroids.storage.unique;
import std.meta;

/// Player component that is unique per world
struct Player {
  /// Whether player ship is accelerating at the moment
  bool thrust;
  /// Counts until moment when player can emit a new bullet
  float fire_cooldown;

  /// Name of player component
  enum name = "player";
  /// Storage type for player. We have only one entity that has the component.
  alias Storage = UniqueStorage!Player;
}

/// Shorthand for enumeration of components that player entity has
alias PlayerComponents = AliasSeq!(
  Player,
  Position,
  Velocity,
  Rotation,
  Mass
);
