#include <stdlib.h>
#include "asteroids/player.h"

int init_player_storage(player_storage *storage) {
  player_storage tmp = (player_storage)malloc(sizeof(struct player_component) * ENTITIES_MAXIMUM_COUNT);
  if (!tmp) {
    asteroids_set_error("Failed to allocate player storage!");
    return 1;
  } else {
    *storage = tmp;
    return 0;
  }
}

void destroy_player_storage(player_storage *storage) {
  if (storage) {
    free(storage);
  }
}

void add_player_component(entity e, struct player_component p, player_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  tag_entity_component(e, COMPONENT_PLAYER, tags);
  *storage[e] = p;
}

void del_player_component(entity e, player_storage *storage, component_tags *tags) {
  if (e < 0 || e > ENTITIES_MAXIMUM_COUNT) {
    return;
  }

  untag_entity_component(e, COMPONENT_PLAYER, tags);
}
