#include <stdlib.h>
#include "asteroids/position.h"

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
    free(storage);
  }
}
