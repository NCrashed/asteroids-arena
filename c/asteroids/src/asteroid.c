#include <stdlib.h>
#include "asteroids/component/asteroid.h"
#include <stdio.h>

int init_asteroid_storage(asteroid_storage *storage) {
  asteroid_storage tmp = (asteroid_storage)malloc(sizeof(struct asteroid_component) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate asteroid storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_asteroid_storage(asteroid_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_asteroid_component(entity e, struct asteroid_component cmp, asteroid_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_ASTEROID, tags);
  (*storage)[e] = cmp;
}

void set_asteroid_component(entity e, struct asteroid_component cmp, asteroid_storage *storage) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }
  (*storage)[e] = cmp;
}

void del_asteroid_component(entity e, asteroid_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_ASTEROID, tags);
}

struct asteroid_component* get_asteroid_component(entity e, asteroid_storage *storage) {
  return &(*storage)[e];
}

const struct asteroid_component* get_asteroid_component_const(entity e, const asteroid_storage *storage) {
  return &(*storage)[e];
}

entity spawn_asteroid
  ( struct asteroid_component asteroid, asteroid_storage *storage
  , struct v2f pos, position_storage *pos_store
  , struct v2f vel, velocity_storage *vel_store
  , float rot, rotation_storage *rot_store
  , float mass, mass_storage *mass_store
  , float radius, radius_storage *radius_store
  , component_tags tags
  , size_t *entity_counter)
{
  entity e = allocate_entity(entity_counter);
  if (e < 0) {
    asteroids_set_error("Failed to allocate new entity for asteroid!");
    return -1;
  }

  add_asteroid_component(e, asteroid, storage, tags);
  add_position_component(e, pos, pos_store, tags);
  add_velocity_component(e, vel, vel_store, tags);
  add_rotation_component(e, rot, rot_store, tags);
  add_mass_component(e, mass, mass_store, tags);
  add_radius_component(e, radius, radius_store, tags);
  return e;
}
