#include "asteroids/system/lifetime.h"

void system_lifetime(entity e
  , bullet_storage *bul_store
  , position_storage *pos_store
  , velocity_storage *vel_store
  , radius_storage *radius_store
  , float dt
  , component_tags tags)
{
  if (entity_has_component(e, COMPONENT_BULLET, tags)) {
    struct bullet_component* bc = get_bullet_component(e, bul_store);
    bc->life_time -= dt;
    if (bc->life_time < 0) {
      destroy_bullet(e, bul_store, pos_store, vel_store, radius_store, tags);
    }
  }
}
