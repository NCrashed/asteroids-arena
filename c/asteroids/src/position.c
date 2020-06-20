#include <stdlib.h>
#include "asteroids/component/position.h"

int init_position_storage(position_storage *storage) {
  position_storage tmp = (position_storage)malloc(sizeof(struct v2f) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate position storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_position_storage(position_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_position_component(entity e, struct v2f pos, position_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_POSITION, tags);
  (*storage)[e] = pos;
}

void set_position_component(entity e, struct v2f pos, position_storage *storage) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  (*storage)[e] = pos;
}

void del_position_component(entity e, position_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_POSITION, tags);
}

struct v2f* get_position_component(entity e, position_storage *storage) {
  return &(*storage)[e];
}

const struct v2f* get_position_component_const(entity e, const position_storage *storage) {
  return &(*storage)[e];
}
