/**
  Module that defines mass component of each entity in the game.
*/
#ifndef ASTEROIDS_MASS_H
#define ASTEROIDS_MASS_H

#include "asteroids/error.h"
#include "asteroids/storage.h"

typedef float * mass_storage;

int init_mass_storage(mass_storage *storage);
void destroy_mass_storage(mass_storage *storage);

void add_mass_component(entity e, float mass, mass_storage *storage, component_tags tags);
void set_mass_component(entity e, float mass, mass_storage *storage);
void del_mass_component(entity e, mass_storage *storage, component_tags tags);

float* get_mass_component(entity e, mass_storage *storage);
const float* get_mass_component_const(entity e, const mass_storage *storage);

#endif /* ASTEROIDS_MASS_H */
