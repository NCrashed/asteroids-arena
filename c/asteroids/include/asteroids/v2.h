/**
  Module that defines 2D vector type and operations.
*/
#ifndef ASTEROIDS_V2_H
#define ASTEROIDS_V2_H

/**
  2D vector of floats
*/
struct v2f {
  float x;
  float y;
} __attribute__((__packed__));

/// Add components of second vector to the firsts one
void v2f_set_add(struct v2f *dest, struct v2f other);

/// Rotate given vector around 0 to given angle
void v2f_rotate(struct v2f *v, float angle);

/// Return squared distance between two points
float v2f_dist_squared(struct v2f v1, struct v2f v2);

#endif /* ASTEROIDS_V2_H */
