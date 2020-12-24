module asteroids.render;

import asteroids.component;
import bindbc.sdl;

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
