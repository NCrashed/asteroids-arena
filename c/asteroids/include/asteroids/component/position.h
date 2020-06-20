/**
  Module that defines position component of each entity in the game.
*/
#ifndef ASTEROIDS_POSITION_H
#define ASTEROIDS_POSITION_H

#include "asteroids/error.h"
#include "asteroids/storage.h"
#include "asteroids/v2.h"

typedef struct v2f * position_storage;

int init_position_storage(position_storage *storage);
void destroy_position_storage(position_storage *storage);

void add_position_component(entity e, struct v2f pos, position_storage *storage, component_tags tags);
void set_position_component(entity e, struct v2f pos, position_storage *storage);
void del_position_component(entity e, position_storage *storage, component_tags tags);

struct v2f* get_position_component(entity e, position_storage *storage);
const struct v2f* get_position_component_const(entity e, const position_storage *storage);

#endif /* ASTEROIDS_POSITION_H */
