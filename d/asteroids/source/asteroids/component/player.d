module asteroids.component.player;

import asteroids.storage.unique;

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
