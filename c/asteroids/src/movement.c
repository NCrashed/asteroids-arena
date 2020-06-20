#include "asteroids/physics/movement.h"

void system_movement(
    entity e
  , position_storage *position
  , const velocity_storage velocity
  , float delta
  , const component_tags tags)
{
  if (entity_has_component(e, COMPONENT_POSITION | COMPONENT_VELOCITY, tags)) {
     v2f_set_add(position[e], velocity[e]);
  }
}
