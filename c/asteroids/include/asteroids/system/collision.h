/**
  System that check collision between entities.
*/
#ifndef ASTEROIDS_COLLISION_H
#define ASTEROIDS_COLLISION_H

#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/radius.h"
#include "asteroids/storage.h"

/// Iterate over all entities to check collision against the given entity.
/// Returns -1 if no entity exists.
/// Second parameter is used to rescan collisions from given another entity,
/// if set to below zero, will scan from begining.
entity system_collision(
    entity e
  , entity prev_collided
  , const position_storage *position
  , const radius_storage *radius
  , size_t entity_counter
  , const component_tags tags);

#endif /* ASTEROIDS_COLLISION_H */
