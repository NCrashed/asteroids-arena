module asteroids.world;

public import asteroids.component;
public import asteroids.storage;

import asteroids.input;
import asteroids.render;
import asteroids.system;
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
  /// Container for all storages of the world supported.
  Storages!AllComponents storages;

  /// Intialize internal storage, allocates memory for them
  this(string sounds_dir) {
    storages.init();
    spawn_player(storages.sub!(Entities, WorldSize, PlayerComponents));
  }

  ///  Make one tick of world simulation with given inputs. Return non zero if failed.
  void step(float dt, in InputEvents events) {
    storages.deltaTime.global = dt;
    applyEvents(dt, events);
    physicsSystem(storages);
  }

  /// Maintain world delayed actions
  void maintain() {
    storages.entities.maintain();
  }

  /// Render world in current frame
  void render(SDL_Renderer* renderer) {
    with(storages) if(!player.unique.isNull) {
      immutable e = player.owner;
      renderPlayer(renderer, player.unique.get, position.get(e), rotation.get(e));
    }
  }

  /// Apply player input to ship components
  private void applyEvents(float dt, InputEvents inputs) {
    with(storages) if(!player.unique.isNull) {
      immutable e = player.owner;
      with(inputs) {
        player.modify(e, (a) { a.thrust = shipThrust; return a; });
        if(shipLeft) rotation.modify(e, a => a - Player.rotationSpeed * dt);
        if(shipRight) rotation.modify(e, a => a + Player.rotationSpeed * dt);
        if(shipThrust) {
          immutable rot = rotation.get(e);
          immutable m = mass.get(e);
          velocity.modify(e, a => a + v2f.fromAngle(rot) * Player.thrustForce * dt / m);
        }
      }
    }
  }
}
