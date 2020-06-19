#include "asteroids/events.h"

void init_input_events(struct input_events* events) {
 events->ship_left = false;
 events->ship_right = false;
 events->ship_thrust = false;
 events->ship_fire = false;
}
