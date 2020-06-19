#include "asteroids/v2.h"

void v2f_set_add(struct v2f *dest, struct v2f other)
{
  dest->x += other.x;
  dest->y += other.y;
}
