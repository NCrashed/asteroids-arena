/**
  System that watch after lifetimes of bullets and destroys them when they are expired.
*/
#ifndef ASTEROIDS_LIFETIME_H
#define ASTEROIDS_LIFETIME_H

#include "asteroids/component/bullet.h"
#include "asteroids/component/position.h"
#include "asteroids/component/velocity.h"
#include "asteroids/component/radius.h"

/// Check the entity if it have velocites and apply them to positions.
/// Also wrap space.
void system_lifetime(entity e
  , bullet_storage *storage
  , position_storage *pos_store
  , velocity_storage *vel_store
  , radius_storage *radius_store
  , float dt
  , component_tags tags);

#endif /* ASTEROIDS_LIFETIME_H */
