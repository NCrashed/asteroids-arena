#include <stdlib.h>
#include "asteroids/velocity.h"

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
    free(storage);
  }
}
