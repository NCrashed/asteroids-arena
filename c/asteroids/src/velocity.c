#include <stdlib.h>
#include "asteroids/component/velocity.h"

int init_velocity_storage(velocity_storage *storage) {
  velocity_storage tmp = (velocity_storage)malloc(sizeof(struct v2f) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate velocity storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_velocity_storage(velocity_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_velocity_component(entity e, struct v2f pos, velocity_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_VELOCITY, tags);
  *storage[e] = pos;
}

void del_velocity_component(entity e, velocity_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_VELOCITY, tags);
}
