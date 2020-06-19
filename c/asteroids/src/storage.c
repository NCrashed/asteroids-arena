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
