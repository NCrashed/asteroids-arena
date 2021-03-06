/**
  System that applies velocities to positions for all entities that have them.
*/
#ifndef ASTEROIDS_MOVEMENT_H
#define ASTEROIDS_MOVEMENT_H

#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"

/// Check the entity if it have velocites and apply them to positions.
/// Also wrap space.
void system_movement(
    entity e
  , position_storage *position
  , const velocity_storage *velocity
  , float delta
  , const component_tags tags);

#endif /* ASTEROIDS_MOVEMENT_H */
