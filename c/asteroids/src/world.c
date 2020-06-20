#include "asteroids/world.h"
#include "asteroids/physics/movement.h"
#include <string.h>

int alloc_world(struct World *world) {
  world->entity_counter = 0;
  memset(world, 0, sizeof(struct World));

  if (init_position_storage(&world->position)) {
    destroy_world(world);
    return 1;
  }
  if (init_velocity_storage(&world->velocity)) {
    destroy_world(world);
    return 1;
  }
  if (init_rotation_storage(&world->rotation)) {
    destroy_world(world);
    return 1;
  }
  if (init_mass_storage(&world->mass)) {
    destroy_world(world);
    return 1;
  }
  if (init_player_storage(&world->player)) {
    destroy_world(world);
    return 1;
  }
  if (init_component_tags(&world->tags)) {
    destroy_world(world);
    return 1;
  }
  return 0;
}

int prepare_world(struct World *world) {
  entity player = world_spawn_player(world
    , (struct player_component) { .thrust = false, .fire_cooldown = 0 }
    , (struct v2f) { .x = WORLD_WIDTH * 0.5, .y = WORLD_HEIGHT * 0.5 }
    , (struct v2f) { .y = 0, .y = 0 }
    , 0
    , PLAYER_MASS );
  if (player < 0) {
    asteroids_set_error("Failed to create initial player!");
    return 1;
  }
  return 0;
}

int init_world(struct World *world) {
  if (alloc_world(world)) {
    return 1;
  }
  prepare_world(world);
  return 0;
}

void destroy_world(struct World *world) {
  if (world->position) destroy_position_storage(&world->position);
  if (world->velocity) destroy_velocity_storage(&world->velocity);
  if (world->rotation) destroy_rotation_storage(&world->rotation);
  if (world->mass) destroy_mass_storage(&world->mass);
  destroy_player_storage(&world->player);
  if (world->tags) destroy_component_tags(&world->tags);
  memset(world, 0, sizeof(struct World));
}

void update_player(struct World *world, float dt) {
  if (world->player.self < 0) return;
  entity pe = world->player.self;

  if (world->player.unique.thrust) {
    float rot = world->rotation[pe];
    float acc = dt * PLAYER_THRUST / world->mass[pe];
    world->velocity[pe].x += cos(rot) * acc;
    world->velocity[pe].y += sin(rot) * acc;
  }
  world->player.unique.thrust = false;
  if (world->player.unique.fire_cooldown > 0) {
    world->player.unique.fire_cooldown -= dt;
  }
}

void apply_events(struct World *world, float dt, const struct input_events *events) {
  if (world->player.self >= 0) {
    entity pe = world->player.self;
    if (events->ship_left) {
      world->rotation[pe] -= PLAYER_ROTATION_SPEED * dt;
    }
    if (events->ship_right) {
      world->rotation[pe] += PLAYER_ROTATION_SPEED * dt;
    }
    if (events->ship_thrust) {
      world->player.unique.thrust = true;
    }
    if (events->ship_fire) {
      if (world->player.unique.fire_cooldown <= 0) {
        // spawn bullet
        world->player.unique.fire_cooldown = PLAYER_FIRE_COOLDOWN;
      }
    }
  }
}

int step_world(struct World *world, float dt, const struct input_events *events) {
  update_player(world, dt);
  apply_events(world, dt, events);
  for (size_t e=0; e < world->entity_counter; e++) {
    system_movement(e, &world->position, world->velocity, dt, world->tags);
  }
  return 0;
}

/// Allocate new player in world. Return -1 if failed.
entity world_spawn_player(struct World *world
  , struct player_component player
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass) {
  return spawn_player(
      player, &world->player
    , position, &world->position
    , velocity, &world->velocity
    , rotation, &world->rotation
    , mass, &world->mass
    , world->tags
    , &world->entity_counter);
}
