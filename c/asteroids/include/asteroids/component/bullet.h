/**
  Module that defines bullet component of each entity in the game.
*/
#ifndef ASTEROIDS_BULLET_H
#define ASTEROIDS_BULLET_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/radius.h"

#define BULLET_SPEED 200
#define BULLET_LIFE_TIME 3
#define BULLET_RADIUS 0.5

/// Marks that the entity is bullet
struct bullet_component {
  /// Amount of time left until despawn
  float life_time;
};

typedef struct bullet_component * bullet_storage;

int init_bullet_storage(bullet_storage *storage);
void destroy_bullet_storage(bullet_storage *storage);

void add_bullet_component(entity e, struct bullet_component cmp, bullet_storage *storage, component_tags tags);
void set_bullet_component(entity e, struct bullet_component cmp, bullet_storage *storage);
void del_bullet_component(entity e, bullet_storage *storage, component_tags tags);

struct bullet_component* get_bullet_component(entity e, bullet_storage *storage);
const struct bullet_component* get_bullet_component_const(entity e, const bullet_storage *storage);

/// Spawn bullet entity with all components. Return -1 if failed.
entity spawn_bullet
  ( struct bullet_component bullet, bullet_storage *storage
  , struct v2f pos, position_storage *pos_store
  , struct v2f vel, velocity_storage *vel_store
  , float radius, radius_storage *radius_store
  , component_tags tags
  , size_t *entity_counter);

#endif /* ASTEROIDS_BULLET_H */
