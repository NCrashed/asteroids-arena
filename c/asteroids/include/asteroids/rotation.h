/**
  Module that defines rotation component of each entity in the game.
*/
#ifndef ASTEROIDS_ROTATION_H
#define ASTEROIDS_ROTATION_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/v2.h"

typedef float * rotation_storage;

int init_rotation_storage(rotation_storage *storage);
void destroy_rotation_storage(rotation_storage *storage);

#endif /* ASTEROIDS_ROTATION_H */
