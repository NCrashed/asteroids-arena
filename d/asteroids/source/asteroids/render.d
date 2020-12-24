module asteroids.render;

import asteroids.component;
import asteroids.math;
import asteroids.v2;
import bindbc.sdl;
import std.math;

@nogc void transformVecs(v2f[] arr, v2f pos, float rot, SDL_Point[] points) {
  foreach(i, v; arr) points[i] = v.transform(pos, rot);
}

@nogc SDL_Point transform(v2f v, v2f pos, float rot)  {
  immutable tv = v.rotate(rot) + pos;
  return SDL_Point(cast(int)tv.x, cast(int)tv.y);
}

void renderPlayer(SDL_Renderer* renderer, Player player, v2f pos, float rot) {
  enum dx = Player.renderSize.x / 2;
  enum dy = Player.renderSize.y / 2;
  enum playerModel = [
    // Bottom line
    v2f( dx,   0),
    v2f(-dx, -dy),
    // Upper line
    v2f( dx,   0),
    v2f(-dx,  dy),
    // Back
    v2f(-dx, -dy),
    v2f(-dx,  dy),
    // Thrust
    v2f(-dx,   -0.5 * dy),
    v2f(-dx-10, 0),
    v2f(-dx,    0.5 * dy),
    v2f(-dx-10, 0),
  ];
  static SDL_Point[] points = new SDL_Point[playerModel.length];
  playerModel.transformVecs(pos, rot, points);
  immutable n = player.thrust ? playerModel.length : playerModel.length-4;
  SDL_RenderDrawLines(renderer, &points[0], n);
}

@nogc void renderAsteroid(SDL_Renderer* renderer, Asteroid asteroid, v2f pos, float rot, float rad) {
  immutable n = asteroid.edges;
  enum maxn = Asteroid.edgesRange[1] * 2;
  static SDL_Point[] points = new SDL_Point[maxn];
  for(int i=0; i <= n-1; i++) {
    points[2*i] = circloidPoint(i, n, rad).transform(pos, rot);
    points[2*i+1] = circloidPoint(i+1, n, rad).transform(pos, rot);
  }

  immutable k = 2*n;
  SDL_RenderDrawLines(renderer, &points[0], k);
}

@nogc private v2f circloidPoint(int i, int n, float r) {
  immutable a = cast(float)i * (2 * PI / cast(float)n);
  float sina = void;
  float cosa = void;
  sincos(a, sina, cosa);
  immutable x = r * (1 + sina * 0.3) * cosa;
  immutable y = r * (1 + cosa * 0.3) * sina;
  return v2f(x, y);
}

void renderBullet(SDL_Renderer* renderer, v2f pos) {
  SDL_RenderDrawPoint(renderer, cast(int)pos.x, cast(int)pos.y);
}
