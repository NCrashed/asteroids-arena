/**
  Code to render player, asteroids and bullets.
*/
#ifndef ASTEROIDS_RENDER_H
#define ASTEROIDS_RENDER_H

#include "asteroids/world.h"
#include <SDL2/SDL.h>

/// Render given entity if it needed to be rendered.
void render_entity(SDL_Renderer *renderer, entity e, const struct World *world);

#endif /* ASTEROIDS_RENDER_H */
