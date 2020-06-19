/**
  User input events that are passed to simulation.
*/
#ifndef ASTEROIDS_EVENTS_H
#define ASTEROIDS_EVENTS_H

#include <stdbool.h>

/// Only object that is controlled by the player is space ship. We consider
/// input events to be "sticky". Ship fires all time while key is down.
struct input_events {
  bool ship_left;
  bool ship_right;
  bool ship_thrust;
  bool ship_fire;
};

/// Setup default values for input events
void init_input_events(struct input_events* events);

#endif /* ASTEROIDS_EVENTS_H */
