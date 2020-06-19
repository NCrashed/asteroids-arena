/**
  Module that defines helpers for operation with component storages.
*/
#ifndef ASTEROIDS_STORAGE_H
#define ASTEROIDS_STORAGE_H

#define ENTITIES_MAXIMUM_COUNT 1024

/// Entity is simple id in array of components.
typedef int entity;

/// Different component types that we can assign to entity. Implementation limits
/// count of components up to 32 for now.
enum component {
  COMPONENT_POSITION
, COMPONENT_ROTATION
, COMPONENT_VELOCITY
, COMPONENT_MASS
, COMPONENT_PLAYER
, COMPONENT_ASTEROID
, COMPONENT_BULLET
};

typedef int * component_tags;

int init_component_tags(component_tags *storage);
void destroy_component_tags(component_tags *storage);

#endif /* ASTEROIDS_STORAGE_H */
