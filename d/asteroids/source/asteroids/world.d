module asteroids.world;

public import asteroids.component;

import asteroids.input;
import asteroids.render;
import asteroids.system;
import bindbc.sdl;
import std.random;

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
  this(string soundsDir) {
    storages.init();
    storages.audio.global.loadFiles(soundsDir);
    storages.rng.global = Rng(Random(unpredictableSeed)); // seeding random number generator
    spawnPlayer(storages.sub!(Entities, WorldSize, PlayerComponents));
    spawnAsteroids(storages.sub!(Entities, Rng, WorldSize, AsteroidComponents));
  }

  ///  Make one tick of world simulation with given inputs. Return non zero if failed.
  void step(float dt, in InputEvents events) {
    storages.deltaTime.global = dt;
    storages.audio.global.updateCooldowns(dt);
    updatePlayer(storages.player, dt);
    applyEvents(dt, events);
    foreach(i, e; storages.entities) {
      physicsSystem(storages, i, e);
      updateBullet(storages.sub!(Entities, Bullet), i, e, dt);
    }
  }

  /// Maintain world delayed actions
  void maintain() {
    storages.entities.maintain();
  }

  /// Render world in current frame
  void render(SDL_Renderer* renderer) {
    with(storages) {
      if(!player.unique.isNull) {
        immutable e = player.owner;
        renderPlayer(renderer, player.unique.get, position.get(e), rotation.get(e));
      }
      foreach(i, e; entities) {
        if(entities.aliveHas!AsteroidComponents(i)) {
          renderAsteroid(renderer, asteroid.get(e), position.get(e), rotation.get(e), radius.get(e));
        } else if (entities.aliveHas!BulletComponents(i)) {
          renderBullet(renderer, position.get(e));
        }
      }
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
          storages.audio.global.play(Sound.thrust);
        }
        if(shipFire && player.unique.get.fireTimer <= 0) {
          spawnBullet(sub!(Entities, BulletComponents), position.get(e), rotation.get(e), velocity.get(e));
          player.unique.get.fireTimer = Player.fireCooldown;
          storages.audio.global.play(Sound.fire);
        }
      }
    }
  }
}
