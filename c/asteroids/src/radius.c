#include <stdlib.h>
#include "asteroids/component/radius.h"

int init_radius_storage(radius_storage *storage) {
  radius_storage tmp = (radius_storage)malloc(sizeof(float) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate radius storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_radius_storage(radius_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_radius_component(entity e, float angle, radius_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_RADIUS, tags);
  (*storage)[e] = angle;
}

void set_radius_component(entity e, float angle, radius_storage *storage) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  (*storage)[e] = angle;
}

void del_radius_component(entity e, radius_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_RADIUS, tags);
}

float* get_radius_component(entity e, radius_storage *storage) {
  return &(*storage)[e];
}

const float* get_radius_component_const(entity e, const radius_storage *storage) {
  return &(*storage)[e];
}
