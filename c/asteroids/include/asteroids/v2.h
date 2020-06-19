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

#endif /* ASTEROIDS_V2_H */
