#include "asteroids/v2.h"
#include <math.h>

void v2f_set_add(struct v2f *dest, struct v2f other)
{
  dest->x += other.x;
  dest->y += other.y;
}

void v2f_rotate(struct v2f *v, float a)
{
  float x = v->x * cos(a) - v->y * sin(a);
  float y = v->x * sin(a) + v->y * cos(a);
  v->x = x;
  v->y = y;
}

float v2f_dist_squared(struct v2f v1, struct v2f v2)
{
  float x = v2.x - v1.x;
  float y = v2.y - v1.y;
  return x*x + y*y;
}

void v2f_scale(struct v2f *v, float scalar)
{
  v->x *= scalar;
  v->y *= scalar;
}
