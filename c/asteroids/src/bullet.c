#include <stdlib.h>
#include "asteroids/component/bullet.h"

int init_bullet_storage(bullet_storage *storage) {
  bullet_storage tmp = (bullet_storage)malloc(sizeof(struct bullet_component) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate bullet storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_bullet_storage(bullet_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_bullet_component(entity e, struct bullet_component cmp, bullet_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_BULLET, tags);
  (*storage)[e] = cmp;
}

void set_bullet_component(entity e, struct bullet_component cmp, bullet_storage *storage) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }
  (*storage)[e] = cmp;
}

void del_bullet_component(entity e, bullet_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_BULLET, tags);
}

struct bullet_component* get_bullet_component(entity e, bullet_storage *storage) {
  return &(*storage)[e];
}

const struct bullet_component* get_bullet_component_const(entity e, const bullet_storage *storage) {
  return &(*storage)[e];
}

entity spawn_bullet
  ( struct bullet_component bullet, bullet_storage *storage
  , struct v2f pos, position_storage *pos_store
  , struct v2f vel, velocity_storage *vel_store
  , float radius, radius_storage *radius_store
  , component_tags tags
  , size_t *entity_counter)
{
  entity e = allocate_entity(entity_counter);
  if (e < 0) {
    asteroids_set_error("Failed to allocate new entity for bullet!");
    return -1;
  }

  add_bullet_component(e, bullet, storage, tags);
  add_position_component(e, pos, pos_store, tags);
  add_velocity_component(e, vel, vel_store, tags);
  add_radius_component(e, radius, radius_store, tags);
  return e;
}
