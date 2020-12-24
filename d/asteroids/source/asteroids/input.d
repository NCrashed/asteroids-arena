module asteroids.input;

/// Only object that is controlled by the player is space ship. We consider
/// input events to be "sticky". Ship fires all time while key is down.
struct InputEvents {
  bool shipLeft = false;
  bool shipRight = false;
  bool shipThrust = false;
  bool shipFire = false;
}
