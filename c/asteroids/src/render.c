#include "asteroids/render.h"

#define PLAYER_DX (0.5 * PLAYER_RENDER_WIDTH)
#define PLAYER_DY (0.5 * PLAYER_RENDER_HEIGHT)

void render_line(SDL_Renderer *renderer, struct v2f pos, float rotation, struct v2f p1, struct v2f p2) {
  v2f_rotate(&p1, rotation);
  v2f_rotate(&p2, rotation);
  SDL_RenderDrawLine(renderer, pos.x + p1.x, pos.y + p1.y
                             , pos.x + p2.x, pos.y + p2.y);
}

struct v2f circloid_point(int i, int n, float r) {
  float a = (float)i * (2 * M_PI / (float)n);
  return (struct v2f) {
      .x = r * (1 + sin(a) * 0.3) * cos(a)
    , .y = r * (1 + cos(a) * 0.3) * sin(a)
    };
}

void render_circloid(SDL_Renderer *renderer, int n, float a0, struct v2f pos, float r) {
  for(int i=0; i <= n-1; i++) {
    render_line(renderer, pos, a0, circloid_point(i, n, r), circloid_point(i+1, n, r));
  }
}

void render_asteroid(SDL_Renderer *renderer, struct asteroid_component asteroid, struct v2f pos, float rotation) {
  render_circloid(renderer, asteroid.edges, rotation, pos, asteroid.radius);
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
    render_player(renderer, world->player.unique
      , *get_position_component_const(e, &world->position)
      , *get_rotation_component_const(e, &world->rotation) );
  } else if (entity_has_component(e, COMPONENT_POSITION | COMPONENT_ROTATION | COMPONENT_ASTEROID, world->tags)) {
    render_asteroid(renderer
      , *get_asteroid_component_const(e, &world->asteroid)
      , *get_position_component_const(e, &world->position)
      , *get_rotation_component_const(e, &world->rotation) );
  }
}
