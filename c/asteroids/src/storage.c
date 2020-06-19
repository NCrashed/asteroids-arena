#include <stdlib.h>
#include "asteroids/error.h"
#include "asteroids/storage.h"

int init_component_tags(component_tags *storage) {
  component_tags tmp = (component_tags)malloc(sizeof(int) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate component tags storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_component_tags(component_tags *storage) {
  if (storage) {
    free(storage);
  }
}

bool entity_has_component(entity e, enum component c, const component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return false;
  }
  int tag = tags[e];
  int ctag = 1 << (int)c;
  return (tag & ctag) == ctag;
}

void tag_entity_component(entity e, enum component c, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }
  *tags[e] = *tags[e] | (1 << (int)c);
}

void untag_entity_component(entity e, enum component c, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }
  *tags[e] = *tags[e] & ~(1 << (int)c);
}
