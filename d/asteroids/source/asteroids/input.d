module asteroids.input;

/// Only object that is controlled by the player is space ship. We consider
/// input events to be "sticky". Ship fires all time while key is down.
struct InputEvents {
  bool ship_left = false;
  bool ship_right = false;
  bool ship_thrust = false;
  bool ship_fire = false;
}
