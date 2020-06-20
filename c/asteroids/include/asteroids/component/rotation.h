/**
  Module that defines rotation component of each entity in the game.
*/
#ifndef ASTEROIDS_ROTATION_H
#define ASTEROIDS_ROTATION_H

#include "asteroids/error.h"
#include "asteroids/storage.h"

typedef float * rotation_storage;

int init_rotation_storage(rotation_storage *storage);
void destroy_rotation_storage(rotation_storage *storage);

void add_rotation_component(entity e, float angle, rotation_storage *storage, component_tags tags);
void del_rotation_component(entity e, rotation_storage *storage, component_tags tags);

float* get_rotation_component(entity e, rotation_storage *storage);
const float* get_rotation_component_const(entity e, const rotation_storage *storage);

#endif /* ASTEROIDS_ROTATION_H */
