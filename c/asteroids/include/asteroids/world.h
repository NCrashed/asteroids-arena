/**
  Module that defines whole game state and storages for different components.
*/
#ifndef ASTEROIDS_WORLD_H
#define ASTEROIDS_WORLD_H

#include <stdlib.h>
#include "asteroids/storage.h"
#include "asteroids/position.h"
#include "asteroids/velocity.h"
#include "asteroids/rotation.h"
#include "asteroids/mass.h"
#include "asteroids/player.h"

/**
  The game uses Entity-Component-System (ECS) design where all game entities
  are decomposed into data pieces called Components. Components are stored
  in structure-of-arrays style and an entity is a simple integer that points
  to the arrays.
*/
struct World {
  size_t entity_counter;
  position_storage position;
  velocity_storage velocity;
  rotation_storage rotation;
  mass_storage mass;
  player_storage player;
  component_tags tags;
};

/// Initialize internal storages, allocates memory for them. Return non zero
/// result on error.
int init_world(struct World *world);

/// Deallocate internal storages and free memory.
void destroy_world(struct World *world);

/// Allocate new player in world. Return -1 if failed.
entity world_spawn_player(struct World *world
  , struct player_component player
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass);

#endif /* ASTEROIDS_WORLD_H */
