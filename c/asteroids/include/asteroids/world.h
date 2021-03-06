/**
  Module that defines whole game state and storages for different components.
*/
#ifndef ASTEROIDS_WORLD_H
#define ASTEROIDS_WORLD_H

#include <stdlib.h>
#include "asteroids/storage.h"
#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/rotation.h"
#include "asteroids/component/mass.h"
#include "asteroids/component/radius.h"
#include "asteroids/component/player.h"
#include "asteroids/component/asteroid.h"
#include "asteroids/component/bullet.h"
#include "asteroids/events.h"
#include "asteroids/sound.h"

#define WORLD_WIDTH 1480
#define WORLD_HEIGHT 1024

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
  radius_storage radius;
  player_storage player;
  asteroid_storage asteroid;
  bullet_storage bullet;
  component_tags tags;
  struct sound_resources *sounds;
};

/// Initialize internal storages, allocates memory for them. Return non zero
/// result on error.
int init_world(struct World *world, struct sound_resources *sounds);

/// Deallocate internal storages and free memory.
void destroy_world(struct World *world);

/// Make one tick of world simulation with given inputs. Return non zero if failed.
int step_world(struct World *world, float dt, const struct input_events *events);

/// Allocate new player in world. Return -1 if failed.
entity world_spawn_player(struct World *world
  , struct player_component player
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass);

/// Allocate new asteroid in world. Return -1 if failed.
entity world_spawn_asteroid(struct World *world
  , struct asteroid_component asteroid
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass
  , float radius );

/// Allocate new asteroids in world. Return -1 if failed.
int world_spawn_asteroid_cracks(struct World *world, entity parent);

/// Allocate new bullet in world. Return -1 if failed.
entity world_spawn_bullet(struct World *world
  , struct bullet_component bullet
  , struct v2f position
  , struct v2f velocity
  , float radius );

/// Spawn random asteroids in world. Return non zero on failure.
int world_spawn_asteroids(struct World *world, size_t num);

#endif /* ASTEROIDS_WORLD_H */
