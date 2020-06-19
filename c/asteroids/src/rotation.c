#include <stdlib.h>
#include "asteroids/rotation.h"

int init_rotation_storage(rotation_storage *storage) {
  rotation_storage tmp = (rotation_storage)malloc(sizeof(float) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate rotation storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_rotation_storage(rotation_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_rotation_component(entity e, float angle, rotation_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_ROTATION, tags);
  *storage[e] = angle;
}

void del_rotation_component(entity e, rotation_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_ROTATION, tags);
}
