module asteroids.component.delta;

import decs.storage.global;

/// Delta time between current frame and previous
struct DeltaTime {
  float dt;
  alias dt this;

  /// Name of component
  enum name = "deltaTime";
  /// We store size as global value
  alias Storage = GlobalStorage!DeltaTime;
}
