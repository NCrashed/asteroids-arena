/**
  Module that defines asteroid component of each entity in the game.
*/
#ifndef ASTEROIDS_ASTEROID_H
#define ASTEROIDS_ASTEROID_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/component/mass.h"
#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/rotation.h"
#include "asteroids/component/radius.h"

#define ASTEROID_EDGES_MIN 8
#define ASTEROID_EDGES_MAX 20
#define ASTEROID_SIZE_MIN 10
#define ASTEROID_SIZE_MAX 130
#define ASTEROID_DENSITY 1
#define ASTEROID_AMOUNT 20
#define ASTEROID_VELOCITY_MIN 0
#define ASTEROID_VELOCITY_MAX 100

/// Marks that the entity is asteroid
struct asteroid_component {
  /// Amount of edges for rendering
  int edges;
};

typedef struct asteroid_component * asteroid_storage;

int init_asteroid_storage(asteroid_storage *storage);
void destroy_asteroid_storage(asteroid_storage *storage);

void add_asteroid_component(entity e, struct asteroid_component cmp, asteroid_storage *storage, component_tags tags);
void set_asteroid_component(entity e, struct asteroid_component cmp, asteroid_storage *storage);
void del_asteroid_component(entity e, asteroid_storage *storage, component_tags tags);

struct asteroid_component* get_asteroid_component(entity e, asteroid_storage *storage);
const struct asteroid_component* get_asteroid_component_const(entity e, const asteroid_storage *storage);

/// Spawn asteroid entity with all components. Return -1 if failed.
entity spawn_asteroid
  ( struct asteroid_component asteroid, asteroid_storage *storage
  , struct v2f pos, position_storage *pos_store
  , struct v2f vel, velocity_storage *vel_store
  , float rot, rotation_storage *rot_store
  , float mass, mass_storage *mass_store
  , float radius, radius_storage *radius_store
  , component_tags tags
  , size_t *entity_counter);

#endif /* ASTEROIDS_ASTEROID_H */
