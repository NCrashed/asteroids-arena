module asteroids.render;

import asteroids.component;
import asteroids.v2;
import bindbc.sdl;
import std.math;

void transform(v2f[] arr, v2f pos, float rot, SDL_Point[] points) {
  foreach(i, v; arr) {
    immutable tv = v.rotate(rot) + pos;
    points[i] = SDL_Point(cast(int)tv.x, cast(int)tv.y);
  }
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
  playerModel.transform(pos, rot, points);
  immutable n = player.thrust ? playerModel.length : playerModel.length-4;
  SDL_RenderDrawLines(renderer, &points[0], n);
}

void renderAsteroid(SDL_Renderer* renderer, Asteroid asteroid, v2f pos, float rot, float rad) {
  immutable n = asteroid.edges;
  enum maxn = Asteroid.edgesRange[1] * 2;
  static v2f[] points = new v2f[maxn];
  for(int i=0; i <= n-1; i++) {
    points[2*i] = circloidPoint(i, n, rad);
    points[2*i+1] = circloidPoint(i+1, n, rad);
  }

  immutable k = 2*n;
  static SDL_Point[] pixPoints = new SDL_Point[maxn];
  points.transform(pos, rot, pixPoints);
  SDL_RenderDrawLines(renderer, &pixPoints[0], k);
}

private v2f circloidPoint(int i, int n, float r) {
  immutable a = cast(float)i * (2 * PI / cast(float)n);
  immutable sina = sin(a);
  immutable cosa = cos(a);
  immutable x = r * (1 + sina * 0.3) * cosa;
  immutable y = r * (1 + cosa * 0.3) * sina;
  return v2f(x, y);
}
