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
    free(storage);
  }
}
