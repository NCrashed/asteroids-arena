module asteroids.component.size;

import decs.storage.global;

/// World size is current size of rendering window in pixels
struct WorldSize {
  int width = 1480;
  int height = 1024;

  /// Name of component
  enum name = "worldSize";
  /// We store size as global value
  alias Storage = GlobalStorage!WorldSize;
}
