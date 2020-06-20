/**
  Module that defines velocity component of each entity in the game.
*/
#ifndef ASTEROIDS_VELOCITY_H
#define ASTEROIDS_VELOCITY_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/v2.h"

typedef struct v2f * velocity_storage;

int init_velocity_storage(velocity_storage *storage);
void destroy_velocity_storage(velocity_storage *storage);

void add_velocity_component(entity e, struct v2f vel, velocity_storage *storage, component_tags tags);
void set_velocity_component(entity e, struct v2f vel, velocity_storage *storage);
void del_velocity_component(entity e, velocity_storage *storage, component_tags tags);

struct v2f* get_velocity_component(entity e, velocity_storage *storage);
const struct v2f* get_velocity_component_const(entity e, const velocity_storage *storage);

#endif /* ASTEROIDS_VELOCITY_H */
