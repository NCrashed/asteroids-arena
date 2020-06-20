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
bool system_collision(
    entity e
  , position_storage *position
  , radius_storage *radius
  , size_t entity_counter
  , const component_tags tags);

#endif /* ASTEROIDS_COLLISION_H */
