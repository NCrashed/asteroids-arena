#include "asteroids/physics/movement.h"

void system_movement(
    position_storage *position
  , const velocity_storage velocity
  , float delta
  , size_t entity_counter
  , const component_tags tags)
{
  for (size_t e=0; e < entity_counter; e++) {
    if (entity_has_component(e, COMPONENT_POSITION | COMPONENT_VELOCITY, tags)) {
       v2f_set_add(position[e], velocity[e]);
    }
  }

}
