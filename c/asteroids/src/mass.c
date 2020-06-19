#include <stdlib.h>
#include "asteroids/component/mass.h"

int init_mass_storage(mass_storage *storage) {
  mass_storage tmp = (mass_storage)malloc(sizeof(float) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate mass storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_mass_storage(mass_storage *storage) {
  if (storage) {
    free(*storage);
  }
}

void add_mass_component(entity e, float mass, mass_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_MASS, tags);
  *storage[e] = mass;
}

void del_mass_component(entity e, mass_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_MASS, tags);
}
