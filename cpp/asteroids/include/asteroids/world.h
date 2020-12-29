#pragma once

#include "v2.h"

#define WORLD_WIDTH 1480
#define WORLD_HEIGHT 1024

#define ASTEROID_EDGES_MIN 8
#define ASTEROID_EDGES_MAX 20
#define ASTEROID_SIZE_MIN 10
#define ASTEROID_SIZE_MAX 130
#define ASTEROID_DENSITY 1
#define ASTEROID_AMOUNT 20
#define ASTEROID_VELOCITY_MIN (-100)
#define ASTEROID_VELOCITY_MAX 100

#define BULLET_SPEED 200.f
#define BULLET_LIFE_TIME 3.f
#define BULLET_RADIUS 1.f

/// Mass of player ship in kg
#define PLAYER_MASS 100000.f
/// Visual X size of ship in meters
#define PLAYER_RENDER_WIDTH 30
/// Visual Y size of ship in meters
#define PLAYER_RENDER_HEIGHT 25
/// Collision radius for player ship in meters
#define PLAYER_COLLIDE_RADIUS 15.f
/// Force of engine in newtons
#define PLAYER_THRUST 20000000
/// Recharge time for bullets in seconds
#define PLAYER_FIRE_COOLDOWN 0.3
/// Rotation speed of ship in radians/seconds
#define PLAYER_ROTATION_SPEED M_PI

struct position : v2f {};
struct velocity : v2f {};
struct rotation { float v; };
struct mass { float v; };
struct radius { float v; };

struct player {
  /// Whether player ship is accelerating at the moment
  bool thrust;
  /// Counts until moment when player can emit a new bullet
  float fire_cooldown;
};

struct asteroid {
  /// Amount of edges for rendering
  int edges;
};

struct bullet {
  /// Amount of time left until despawn
  float life_time;
};
