#include "asteroids/render.h"

#define PLAYER_DX (0.5 * PLAYER_RENDER_WIDTH)
#define PLAYER_DY (0.5 * PLAYER_RENDER_HEIGHT)

void render_line(SDL_Renderer *renderer, struct v2f pos, float rotation, struct v2f p1, struct v2f p2) {
  v2f_rotate(&p1, rotation);
  v2f_rotate(&p2, rotation);
  SDL_RenderDrawLine(renderer, pos.x + p1.x, pos.y + p1.y
                             , pos.x + p2.x, pos.y + p2.y);
}

void render_player(SDL_Renderer *renderer, struct player_component player, struct v2f pos, float rotation) {
  render_line(renderer, pos, rotation, (struct v2f) { .x =  PLAYER_DX, .y =  0 }
                                     , (struct v2f) { .x = -PLAYER_DX, .y = -PLAYER_DY});
  render_line(renderer, pos, rotation, (struct v2f) { .x =  PLAYER_DX, .y =  0 }
                                     , (struct v2f) { .x = -PLAYER_DX, .y =  PLAYER_DY});
  render_line(renderer, pos, rotation, (struct v2f) { .x = -PLAYER_DX, .y = -PLAYER_DY }
                                     , (struct v2f) { .x = -PLAYER_DX, .y =  PLAYER_DY});
  if (player.thrust) {
      render_line(renderer, pos, rotation, (struct v2f) { .x = -PLAYER_DX   , .y = -0.5*PLAYER_DY }
                                         , (struct v2f) { .x = -PLAYER_DX-10, .y =  0});
      render_line(renderer, pos, rotation, (struct v2f) { .x = -PLAYER_DX   , .y =  0.5*PLAYER_DY }
                                         , (struct v2f) { .x = -PLAYER_DX-10, .y =  0});
  }
}

void render_entity(SDL_Renderer *renderer, entity e, const struct World *world) {
  if (entity_has_component(e, COMPONENT_POSITION | COMPONENT_ROTATION | COMPONENT_PLAYER, world->tags)) {
     render_player(renderer, world->player.unique, world->position[e], world->rotation[e]);
  }
}
