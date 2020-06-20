#include "asteroids/system/collision.h"

entity system_collision(
    entity e1
  , const position_storage *position
  , const radius_storage *radius
  , size_t entity_counter
  , const component_tags tags)
{
  if (entity_has_component(e1, COMPONENT_POSITION | COMPONENT_RADIUS, tags)) {
    const struct v2f *p1 = get_position_component_const(e1, position);
    float r1 = *get_radius_component_const(e1, radius);
    for (size_t e2=e1+1; e2 < entity_counter; e2++) {
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
