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
  if (init_rotation_storage(&world->rotation)) {
    destroy_velocity_storage(&world->velocity);
    destroy_position_storage(&world->position);
    return 1;
  }
  if (init_player_storage(&world->player)) {
    destroy_rotation_storage(&world->rotation);
    destroy_velocity_storage(&world->velocity);
    destroy_position_storage(&world->position);
    return 1;
  }
  if (init_component_tags(&world->tags)) {
    destroy_player_storage(&world->player);
    destroy_rotation_storage(&world->rotation);
    destroy_velocity_storage(&world->velocity);
    destroy_position_storage(&world->position);
    return 1;
  }
  return 0;
}

void destroy_world(struct World *world) {
  destroy_position_storage(&world->position);
  destroy_velocity_storage(&world->velocity);
  destroy_rotation_storage(&world->rotation);
  destroy_player_storage(&world->player);
  destroy_component_tags(&world->tags);
}
