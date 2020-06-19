/**
  Module that defines player component of an entity in the game.
*/
#ifndef ASTEROIDS_PLAYER_H
#define ASTEROIDS_PLAYER_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/rotation.h"
#include "asteroids/component/mass.h"

struct player_component {
  /// Whether player ship is accelerating at the moment
  bool thrust;
  /// Counts until moment when player can emit a new bullet
  float fire_cooldown;
};

typedef struct player_component* player_storage;

int init_player_storage(player_storage *storage);
void destroy_player_storage(player_storage *storage);

void add_player_component(entity e, struct player_component player, player_storage *storage, component_tags *tags);
void del_player_component(entity e, player_storage *storage, component_tags *tags);

/// Spawn new player entity and return new entity.
entity spawn_player(
    struct player_component player, player_storage *storage
  , struct v2f position, position_storage *pos_storage
  , struct v2f velocity, velocity_storage *vel_storage
  , float rotation, rotation_storage *rot_storage
  , float mass, mass_storage *m_storage
  , component_tags *tags
  , size_t *entity_counter );

#endif /* ASTEROIDS_PLAYER_H */
