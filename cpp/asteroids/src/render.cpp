#include "asteroids/game.h"

#include "asteroids/world.h"

#include <entt/entt.hpp>
#include <SDL2/SDL.h>

#define PLAYER_DX (0.5 * PLAYER_RENDER_WIDTH)
#define PLAYER_DY (0.5 * PLAYER_RENDER_HEIGHT)

#define MAX_EDGES (2 * ASTEROID_EDGES_MAX)

namespace {

void render_line(SDL_Renderer *renderer, const v2f &pos, float rotation, v2f p1, v2f p2) {
  p1.rotate(rotation);
  p2.rotate(rotation);
  SDL_RenderDrawLine(renderer, pos.x + p1.x, pos.y + p1.y
                             , pos.x + p2.x, pos.y + p2.y);
}

void render_lines(SDL_Renderer *renderer, const v2f &pos, float rotation, std::vector<v2f> &ps, int n) {
  static std::vector<SDL_Point> lines_i(MAX_EDGES);
  for (int i = 0; i < n; i++) {
    auto p = ps[i];
    p.rotate(rotation);
    p += pos;
    lines_i[i].x = p.x;
    lines_i[i].y = p.y;
  }
  SDL_RenderDrawLines(renderer, lines_i.data(), n);
}

v2f circloid_point(int i, int n, float r) {
  float a = (float)i * (2 * M_PI / (float)n);
  return {
      r * (1.f + (float)sin(a) * 0.3f) * (float)cos(a)
    , r * (1.f + (float)cos(a) * 0.3f) * (float)sin(a)
  };
}

void render_circloid(SDL_Renderer *renderer, int n, float rotation, const position &pos, float r) {
  static std::vector<v2f> lines_f(MAX_EDGES);
  for (int i = 0; i < n; i++) {
    lines_f[2 * i] = circloid_point(i, n, r);
    lines_f[2 * i + 1] = circloid_point(i + 1, n, r);
  }
  render_lines(renderer, pos, rotation, lines_f, 2 * n);
}

void render_asteroid(SDL_Renderer *renderer, const asteroid &a, const position &pos, float rotation, float radius) {
  render_circloid(renderer, a.edges, rotation, pos, radius);
}

void render_player(SDL_Renderer *renderer, player &player, const position &pos, float rotation) {
  render_line(renderer, pos, rotation, {  PLAYER_DX,  0 }
                                     , { -PLAYER_DX, -PLAYER_DY });
  render_line(renderer, pos, rotation, {  PLAYER_DX,  0 }
                                     , { -PLAYER_DX,  PLAYER_DY });
  render_line(renderer, pos, rotation, { -PLAYER_DX, -PLAYER_DY }
                                     , { -PLAYER_DX,  PLAYER_DY });
  if (player.thrust) {
    render_line(renderer, pos, rotation, { -PLAYER_DX     , -0.5 * PLAYER_DY }
                                       , { -PLAYER_DX - 10,  0 });
    render_line(renderer, pos, rotation, { -PLAYER_DX     ,  0.5 * PLAYER_DY }
                                       , { -PLAYER_DX - 10,  0 });
  }
}

void render_bullet(SDL_Renderer *renderer, const position &pos) {
  SDL_RenderDrawPoint(renderer, pos.x, pos.y);
}

} // namespace

void game::render(SDL_Renderer *renderer) {
  auto player_view = registry.view<player>();
  for (auto entity : player_view) {
    auto &pl = registry.get<player>(entity);
    auto &pos = registry.get<position>(entity);
    auto &rot = registry.get<rotation>(entity);
    render_player(renderer, pl, pos, rot.v);
  }

  auto asteroid_view = registry.view<asteroid>();
  for (auto entity : asteroid_view) {
    auto &a = asteroid_view.get<asteroid>(entity);
    auto &pos = registry.get<position>(entity);
    auto &rot = registry.get<rotation>(entity);
    auto &rad = registry.get<radius>(entity);
    render_asteroid(renderer, a, pos, rot.v, rad.v);
  }

  auto bullet_view = registry.view<bullet>();
  for (auto entity : bullet_view) {
    auto &pos = registry.get<position>(entity);
    render_bullet(renderer, pos);
  }
}
