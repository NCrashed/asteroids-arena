#include <stdlib.h>
#include "asteroids/component/player.h"

int init_player_storage(player_storage *storage) {
  storage->unique = (struct player_component) { .thrust = false, .fire_cooldown = 0 };
  storage->self = -1;
  return 0;
}

void destroy_player_storage(player_storage *storage) {

}

void add_player_component(entity e, struct player_component p, player_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_PLAYER, tags);
  storage->unique = p;
  storage->self = e;
}

void set_player_component(entity e, struct player_component p, player_storage *storage) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  storage->unique = p;
  storage->self = e;
}

void del_player_component(entity e, player_storage *storage, component_tags tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_PLAYER, tags);
  storage->self = -1;
}

entity spawn_player(
      struct player_component player, player_storage *storage
    , struct v2f position, position_storage *pos_storage
    , struct v2f velocity, velocity_storage *vel_storage
    , float rotation, rotation_storage *rot_storage
    , float mass, mass_storage *m_storage
    , float radius, radius_storage *r_storage
    , component_tags tags
    , size_t *entity_counter )
{
  entity e = allocate_entity(entity_counter);
  if (e < 0) {
    asteroids_set_error("Failed to allocate new entity for player!");
    return -1;
  }

  add_player_component(e, player, storage, tags);
  add_position_component(e, position, pos_storage, tags);
  add_velocity_component(e, velocity, vel_storage, tags);
  add_rotation_component(e, rotation, rot_storage, tags);
  add_mass_component(e, mass, m_storage, tags);
  add_radius_component(e, radius, r_storage, tags);
  return e;
}

void respawn_player(
    entity e
  , struct player_component player, player_storage *storage
  , struct v2f position, position_storage *pos_storage
  , struct v2f velocity, velocity_storage *vel_storage
  , float rotation, rotation_storage *rot_storage )
{
  set_player_component(e, player, storage);
  set_position_component(e, position, pos_storage);
  set_velocity_component(e, velocity, vel_storage);
  set_rotation_component(e, rotation, rot_storage);
}

struct player_component* get_player_component(entity e, player_storage *storage) {
  return &storage->unique;
}

const struct player_component* get_player_component_const(entity e, const player_storage *storage) {
  return &storage->unique;
}
