/**
  System that applies velocities to positions for all entities that have them.
*/
#ifndef ASTEROIDS_MOVEMENT_H
#define ASTEROIDS_MOVEMENT_H

#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"

/// Iterate over all entities that have velocites and apply them to positions.
void system_movement(
    entity e
  , position_storage *position
  , const velocity_storage velocity
  , float delta
  , const component_tags tags);

#endif /* ASTEROIDS_MOVEMENT_H */
