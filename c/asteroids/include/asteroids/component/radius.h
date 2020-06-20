/**
  Module that defines radius component for collisions of each entity in the game.
*/
#ifndef ASTEROIDS_RADIUS_H
#define ASTEROIDS_RADIUS_H

#include "asteroids/error.h"
#include "asteroids/storage.h"

typedef float * radius_storage;

int init_radius_storage(radius_storage *storage);
void destroy_radius_storage(radius_storage *storage);

void add_radius_component(entity e, float angle, radius_storage *storage, component_tags tags);
void del_radius_component(entity e, radius_storage *storage, component_tags tags);

float* get_radius_component(entity e, radius_storage *storage);
const float* get_radius_component_const(entity e, const radius_storage *storage);

#endif /* ASTEROIDS_RADIUS_H */
