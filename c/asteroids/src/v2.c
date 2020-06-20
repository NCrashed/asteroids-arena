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
  float y = v->x * sin(a) - v->y * cos(a);
  v->x = x;
  v->y = y;
}
