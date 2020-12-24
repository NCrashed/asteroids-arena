module asteroids.component.audio;

import asteroids.storage.global;

/// Global component that holds audio resources in memory
struct Audio {
  /// Name of component
  enum name = "audio";
  /// We store size as global value
  alias Storage = GlobalStorage!Audio;
}
