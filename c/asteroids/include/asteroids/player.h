/**
  Module that defines player component of an entity in the game.
*/
#ifndef ASTEROIDS_PLAYER_H
#define ASTEROIDS_PLAYER_H

#include "asteroids/error.h"
#include "asteroids/storage.h"

struct player_component {
  /// Whether player ship is accelerating at the moment
  bool thrust;
  /// Counts until moment when player can emit a new bullet
  float fire_cooldown;
};

typedef struct player_component* player_storage;

int init_player_storage(player_storage *storage);
void destroy_player_storage(player_storage *storage);

void add_player_component(entity e, struct player_component pos, player_storage *storage, component_tags *tags);
void del_player_component(entity e, player_storage *storage, component_tags *tags);

#endif /* ASTEROIDS_PLAYER_H */
