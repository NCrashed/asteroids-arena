/**
  Module that defines helpers for operation with component storages.
*/
#ifndef ASTEROIDS_STORAGE_H
#define ASTEROIDS_STORAGE_H

#include <stdbool.h>
#include <stdlib.h>

#define ENTITIES_MAXIMUM_COUNT 1024

/// Entity is simple id in array of components.
typedef int entity;

/// Different component types that we can assign to entity. Implementation limits
/// count of components up to 32 for now.
enum component {
  COMPONENT_POSITION = 1
, COMPONENT_ROTATION = 2
, COMPONENT_VELOCITY = 4
, COMPONENT_MASS     = 8
, COMPONENT_RADIUS   = 16
, COMPONENT_PLAYER   = 32
, COMPONENT_ASTEROID = 64
, COMPONENT_BULLET   = 128
};

typedef int * component_tags;

int init_component_tags(component_tags *storage);
void destroy_component_tags(component_tags *storage);

/// Check whether the entity has associated component. The second argument is
/// combined `component` tags.
bool entity_has_component(entity e, int components, const component_tags tags);
/// Mark that entity has given component. The function doesn't touch actuall component data.
void tag_entity_component(entity e, enum component c, component_tags tags);
/// Mark that entity doesn't have given component. The function doesn't touch actuall component data.
void untag_entity_component(entity e, enum component c, component_tags tags);

/// Allocate new entity (increasing the counter) and return -1 if failed to do so.
entity allocate_entity(size_t *entity_counter);

#endif /* ASTEROIDS_STORAGE_H */
