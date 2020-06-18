/**
  Module that defines position component of each entity in the game.
*/
#ifndef ASTEROIDS_POSITION_H
#define ASTEROIDS_POSITION_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/v2.h"

typedef struct v2f * position_storage;

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

#endif /* ASTEROIDS_POSITION_H */
