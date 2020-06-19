/**
  Module that defines position component of each entity in the game.
*/
#ifndef ASTEROIDS_POSITION_H
#define ASTEROIDS_POSITION_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/v2.h"

typedef struct v2f * position_storage;

int init_position_storage(position_storage *storage);
void destroy_position_storage(position_storage *storage);

#endif /* ASTEROIDS_POSITION_H */
