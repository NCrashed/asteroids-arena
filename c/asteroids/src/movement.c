#include "asteroids/physics/movement.h"
#include "asteroids/world.h"

void system_movement(
    entity e
  , position_storage *position
  , const velocity_storage *velocity
  , float delta
  , const component_tags tags)
{
  if (entity_has_component(e, COMPONENT_POSITION | COMPONENT_VELOCITY, tags)) {
    struct v2f *p = get_position_component(e, position);
    const struct v2f *v = get_velocity_component_const(e, velocity);
    p->x += v->x * delta;
    p->y += v->y * delta;
  }
  if (entity_has_component(e, COMPONENT_POSITION, tags)) {
    struct v2f *p = get_position_component(e, position);
    if (p->x < 0) p->x += WORLD_WIDTH;
    if (p->x > WORLD_WIDTH) p->x -= WORLD_WIDTH;
    if (p->y < 0) p->y += WORLD_HEIGHT;
    if (p->y > WORLD_HEIGHT) p->y -= WORLD_HEIGHT;
  }
}
