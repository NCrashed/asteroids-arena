#include "asteroids/system/collision.h"

entity system_collision(
    entity e1
  , entity prev_collided
  , const position_storage *position
  , const radius_storage *radius
  , size_t entity_counter
  , const component_tags tags)
{
  size_t e2start = e1+1;
  if (prev_collided >= 0) {
    e2start = prev_collided+1;
  }

  if (entity_has_component(e1, COMPONENT_POSITION | COMPONENT_RADIUS, tags)) {
    const struct v2f *p1 = get_position_component_const(e1, position);
    float r1 = *get_radius_component_const(e1, radius);
    for (size_t e2=e2start; e2 < entity_counter; e2++) {
      if(entity_has_component(e2, COMPONENT_POSITION | COMPONENT_RADIUS, tags)) {
        const struct v2f* p2 = get_position_component_const(e2, position);
        float r2 = *get_radius_component_const(e2, radius);
        float distsq = v2f_dist_squared(*p1, *p2);
        float r = r1 + r2;
        if (distsq < r * r) {
          return e2;
        }
      }
    }
  }
  return -1;
}
