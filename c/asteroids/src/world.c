#include "asteroids/world.h"
#include "asteroids/system/movement.h"
#include "asteroids/system/collision.h"
#include "asteroids/system/lifetime.h"
#include <string.h>
#include <SDL2/SDL.h>

int alloc_world(struct World *world, struct sound_resources *sounds) {
  memset(world, 0, sizeof(struct World));
  world->sounds = sounds;

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
  if (init_radius_storage(&world->radius)) {
    destroy_world(world);
    return 1;
  }
  if (init_player_storage(&world->player)) {
    destroy_world(world);
    return 1;
  }
  if (init_asteroid_storage(&world->asteroid)) {
    destroy_world(world);
    return 1;
  }
  if (init_bullet_storage(&world->bullet)) {
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
    , (struct v2f) { .x = 0, .y = 0 }
    , 0
    , PLAYER_MASS );
  if (player < 0) {
    asteroids_set_error("Failed to create initial player!");
    return 1;
  }
  if (world_spawn_asteroids(world, ASTEROID_AMOUNT)) {
    return 1;
  }
  return 0;
}

int init_world(struct World *world, struct sound_resources *sounds) {
  if (alloc_world(world, sounds)) {
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
  if (world->radius) destroy_radius_storage(&world->radius);
  destroy_player_storage(&world->player);
  if (world->asteroid) destroy_asteroid_storage(&world->asteroid);
  if (world->bullet) destroy_bullet_storage(&world->bullet);
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
      play_sound(world->sounds, world->sounds->thrust, 0, 0.35);
    }
    if (events->ship_fire) {
      if (world->player.unique.fire_cooldown <= 0) {
        play_sound(world->sounds, world->sounds->fire, 1, 0.2);
        struct v2f bpos = *get_position_component(pe, &world->position);
        struct v2f bvel = get_player_direction(pe, &world->rotation);
        v2f_scale(&bvel, BULLET_SPEED);
        v2f_set_add(&bvel, *get_velocity_component(pe, &world->velocity));

        entity be = world_spawn_bullet(world
          , (struct bullet_component) { .life_time = BULLET_LIFE_TIME }
          , bpos, bvel, BULLET_RADIUS);
        if (be < 0) {
          SDL_Log("Failed to spawn player bullet: %s\n", asteroids_get_error());
        }
        world->player.unique.fire_cooldown = PLAYER_FIRE_COOLDOWN;
      }
    }
  }
}

void world_respawn_player(struct World *world, entity e) {
  play_sound(world->sounds, world->sounds->bang_medium, 2, 0.3);
  respawn_player(e
    , (struct player_component) { .thrust = false, .fire_cooldown = PLAYER_FIRE_COOLDOWN }, &world->player
    , (struct v2f) { .x = WORLD_WIDTH * 0.5, .y = WORLD_HEIGHT * 0.5 }, &world->position
    , (struct v2f) { .x = 0, .y = 0 }, &world->velocity
    , 0, &world->rotation );
}

void world_break_asteroid(struct World *world, entity bullet_entity, entity asteroid_entity) {
  play_sound(world->sounds, world->sounds->bang_medium, 2, 0.2);
  destroy_bullet(bullet_entity, &world->bullet, &world->position, &world->velocity, &world->radius, world->tags);
  world_spawn_asteroid_cracks(world, asteroid_entity);
  destroy_asteroid(asteroid_entity, &world->asteroid, &world->position, &world->velocity, &world->rotation, &world->radius, &world->mass, world->tags);
}

int step_world(struct World *world, float dt, const struct input_events *events) {
  update_player(world, dt);
  apply_events(world, dt, events);
  for (size_t e=0; e < world->entity_counter; e++) {
    system_lifetime(e, &world->bullet, &world->position, &world->velocity, &world->radius, dt, world->tags);
    system_movement(e, &world->position, &world->velocity, dt, world->tags);
    entity old_ecoll = -1;
    entity ecoll = -1;
    do {
      ecoll = system_collision(e, old_ecoll, &world->position, &world->radius, world->entity_counter, world->tags);
      if (ecoll >= 0) {
        if (entity_has_component(e, COMPONENT_PLAYER, world->tags) && entity_has_component(ecoll, COMPONENT_ASTEROID, world->tags)) {
          world_respawn_player(world, e);
        } else if (entity_has_component(e, COMPONENT_ASTEROID, world->tags) && entity_has_component(ecoll, COMPONENT_PLAYER, world->tags)) {
          world_respawn_player(world, ecoll);
        } else if (entity_has_component(e, COMPONENT_BULLET, world->tags) && entity_has_component(ecoll, COMPONENT_ASTEROID, world->tags)) {
          world_break_asteroid(world, e, ecoll);
        } else if (entity_has_component(e, COMPONENT_ASTEROID, world->tags) && entity_has_component(ecoll, COMPONENT_BULLET, world->tags)) {
          world_break_asteroid(world, ecoll, e);
        }
      }
      old_ecoll = ecoll;
    } while (ecoll >= 0);
  }
  return 0;
}

float randf(float min, float max) {
  return min + (float)rand()/(float)(RAND_MAX/(max - min));
}

int randi(int min, int max) {
  return min + (int)((float)rand()/(RAND_MAX/(float)(max - min)));
}

struct v2f random_asteroid_velocity(struct v2f v) {
  return (struct v2f) { .x = v.x + randf(ASTEROID_VELOCITY_MIN, ASTEROID_VELOCITY_MAX), .y = v.y + randf(ASTEROID_VELOCITY_MIN, ASTEROID_VELOCITY_MAX) };
}

int world_spawn_asteroid_cracks(struct World *world, entity parent) {
  float r = 0.5 * (*get_radius_component(parent, &world->radius));
  if (r < ASTEROID_SIZE_MIN) return 0;

  struct v2f pos = *get_position_component(parent, &world->position);
  struct v2f vel = *get_velocity_component(parent, &world->velocity);

  float m = ASTEROID_DENSITY * M_PI * r * r;
  entity a1 = world_spawn_asteroid(world
    , (struct asteroid_component) { .edges = randi(ASTEROID_EDGES_MIN, ASTEROID_EDGES_MAX) }
    , pos, random_asteroid_velocity(vel), randf(0, 2*M_PI), m, r);
  if (a1 < 0) {
    SDL_Log("Failed to crack asteroid: %s\n", asteroids_get_error());
    return -1;
  }
  entity a2 = world_spawn_asteroid(world
    , (struct asteroid_component) { .edges = randi(ASTEROID_EDGES_MIN, ASTEROID_EDGES_MAX) }
    , pos, random_asteroid_velocity(vel), randf(0, 2*M_PI), m, r);
  if (a2 < 0) {
    SDL_Log("Failed to crack asteroid: %s\n", asteroids_get_error());
    return -1;
  }
  return 0;
}

/// Allocate new player in world. Return -1 if failed.
entity world_spawn_player(struct World *world
  , struct player_component player
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass)
{
  return spawn_player(
      player, &world->player
    , position, &world->position
    , velocity, &world->velocity
    , rotation, &world->rotation
    , mass, &world->mass
    , PLAYER_COLLIDE_RADIUS, &world->radius
    , world->tags
    , &world->entity_counter);
}

/// Allocate new player in world. Return -1 if failed.
entity world_spawn_asteroid(struct World *world
  , struct asteroid_component asteroid
  , struct v2f position
  , struct v2f velocity
  , float rotation
  , float mass
  , float radius )
{
  return spawn_asteroid(
      asteroid, &world->asteroid
    , position, &world->position
    , velocity, &world->velocity
    , rotation, &world->rotation
    , radius, &world->radius
    , mass, &world->mass
    , world->tags
    , &world->entity_counter);
}

/// Allocate new bullet in world. Return -1 if failed.
entity world_spawn_bullet(struct World *world
  , struct bullet_component bullet
  , struct v2f position
  , struct v2f velocity
  , float radius )
{
  return spawn_bullet(
      bullet, &world->bullet
    , position, &world->position
    , velocity, &world->velocity
    , radius, &world->radius
    , world->tags
    , &world->entity_counter);
}

int world_spawn_asteroids(struct World *world, size_t num) {
  for (size_t i=0; i < num; i++) {
    float radius = randf(ASTEROID_SIZE_MIN, ASTEROID_SIZE_MAX);
    entity e = world_spawn_asteroid
      (  world
      , (struct asteroid_component) { .edges = randi(ASTEROID_EDGES_MIN, ASTEROID_EDGES_MAX) }
      , (struct v2f) { .x = randf(0, WORLD_WIDTH), .y = randf(0, WORLD_HEIGHT) }
      , random_asteroid_velocity((struct v2f) { .x = 0, .y = 0 })
      , randf(0, 2*M_PI)
      , ASTEROID_DENSITY * M_PI * radius * radius
      , radius
      );
    if (e < 0) {
      return 1;
    }
  }
  return 0;
}
