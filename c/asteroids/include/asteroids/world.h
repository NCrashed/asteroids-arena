/**
  Module that defines whole game state and storages for different components.
*/
#ifndef ASTEROIDS_WORLD_H
#define ASTEROIDS_WORLD_H

/**
  The game uses Entity-Component-System (ECS) design where all game entities
  are decomposed into data pieces called Components. Components are stored
  in structure-of-arrays style and an entity is a simple integer that points
  to the arrays.
*/
struct World {
  size_t entity_counter;
};

/// Initialize internal storages, allocates memory for them. Return non zero
/// result on error.
int init_world(struct World *world) {
  world->entity_counter = 0;
  return 0;
}

/// Deallocate internal storages and free memory.
void destroy_world(struct World *world) {

}

#endif /* ASTEROIDS_WORLD_H */
