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
#include "math.h"

/// Mass of player ship in kg
#define PLAYER_MASS 100000
/// Visual X size of ship in meters
#define PLAYER_RENDER_WIDTH 30
/// Visual Y size of ship in meters
#define PLAYER_RENDER_HEIGHT 25
/// Collision radius for player ship in meters
#define PLAYER_COLLIDE_RADIUS 15
/// Force of engine in newtons
#define PLAYER_THRUST 20000000
/// Recharge time for bullets in seconds
#define PLAYER_FIRE_COOLDOWN 0.3
/// Rotation speed of ship in radians/seconds
#define PLAYER_ROTATION_SPEED M_PI

struct player_component {
  /// Whether player ship is accelerating at the moment
  bool thrust;
  /// Counts until moment when player can emit a new bullet
  float fire_cooldown;
};

/// We have a single player. So any entity with player component has the same
/// data for player component.
typedef struct player_storage_t {
  //// We have single unique component for all entities
  struct player_component unique;
  /// Self reference to find other components associated with the player
  entity self;
} player_storage;

int init_player_storage(player_storage *storage);
void destroy_player_storage(player_storage *storage);

void add_player_component(entity e, struct player_component player, player_storage *storage, component_tags tags);
void del_player_component(entity e, player_storage *storage, component_tags tags);

struct player_component* get_player_component(entity e, player_storage *storage);
const struct player_component* get_player_component_const(entity e, const player_storage *storage);

/// Spawn new player entity and return new entity.
entity spawn_player(
    struct player_component player, player_storage *storage
  , struct v2f position, position_storage *pos_storage
  , struct v2f velocity, velocity_storage *vel_storage
  , float rotation, rotation_storage *rot_storage
  , float mass, mass_storage *m_storage
  , component_tags tags
  , size_t *entity_counter );

#endif /* ASTEROIDS_PLAYER_H */
