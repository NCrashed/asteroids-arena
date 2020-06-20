#include "asteroids/physics/movement.h"
#include "asteroids/world.h"

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
  if (entity_has_component(e, COMPONENT_POSITION, tags)) {
    struct v2f *p = position[e];
    if (p->x < 0) p->x += WORLD_WIDTH;
    if (p->x > WORLD_WIDTH) p->x -= WORLD_WIDTH;
    if (p->y < 0) p->y += WORLD_HEIGHT;
    if (p->y > WORLD_HEIGHT) p->y -= WORLD_HEIGHT;
  }
}
