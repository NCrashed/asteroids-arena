module asteroids.world;

public import asteroids.component;

import asteroids.input;
import asteroids.storage.entity;
import asteroids.system.player;
import bindbc.sdl;
import std.experimental.allocator;

/// Initial world width in pixels
immutable initial_width = WorldSize().width;
/// Initial world height in pixels
immutable initial_height = WorldSize().height;

/// The game uses Entity-Component-System (ECS) design where all game entities
/// are decomposed into data pieces called Components. Components are stored
/// in structure-of-arrays style and an entity is a simple integer that points
/// to the arrays.
///
/// Type argument is components that are supported by the world type.
class World(T...) {
  // Embed all storages for each component type
  mixin Components!T.Storages;

  /// Intialize internal storage, allocates memory for them
  this(string sounds_dir) {

  }

  ///  Make one tick of world simulation with given inputs. Return non zero if failed.
  void step(float dt, in InputEvents events) {

  }

  /// Maintain world delayed actions
  void maintain() {
    entities.maintain();
  }

  /// Render world in current frame
  void render(SDL_Renderer* renderer) {

  }
}
