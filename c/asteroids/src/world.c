#include "asteroids/world.h"

int init_world(struct World *world) {
  world->entity_counter = 0;
  if (init_position_storage(&world->position)) {
    return 1;
  }
  if (init_velocity_storage(&world->velocity)) {
    destroy_position_storage(&world->position);
    return 1;
  }
  return 0;
}


void destroy_world(struct World *world) {
  destroy_position_storage(&world->position);
}
