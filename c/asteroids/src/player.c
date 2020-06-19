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
