module asteroids.world;

public import asteroids.component;
public import asteroids.storage;

import asteroids.input;
import asteroids.render;
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
class World {
  alias CS = Components!AllComponents;
  // Embed all storages for each component type
  mixin CS.Storages;

  /// Intialize internal storage, allocates memory for them
  this(string sounds_dir) {
    mixin(CS.initStorages());
    spawn_player(mixin(CS.collect!(Entities, WorldSize, PlayerComponents)()));
  }

  ///  Make one tick of world simulation with given inputs. Return non zero if failed.
  void step(float dt, in InputEvents events) {
    applyEvents(dt, events);
  }

  /// Maintain world delayed actions
  void maintain() {
    entities.maintain();
  }

  /// Render world in current frame
  void render(SDL_Renderer* renderer) {
    if(!player.unique.isNull) {
      immutable e = player.owner;
      renderPlayer(renderer, player.unique.get, position.get(e), rotation.get(e));
    }
  }

  /// Apply player input to ship components
  private void applyEvents(float dt, InputEvents inputs) {
    if(!player.unique.isNull) {
      immutable e = player.owner;
      with(inputs) {
        if(shipLeft) rotation.modify(e, a => a - Player.rotationSpeed * dt);
        if(shipRight) rotation.modify(e, a => a + Player.rotationSpeed * dt);
        player.modify(e, (a) { a.thrust = shipThrust; return a; });
      }
    }
  }
}
