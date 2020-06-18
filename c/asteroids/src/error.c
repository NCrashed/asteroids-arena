#include <stdbool.h>
#include <string.h>
#include "asteroids/error.h"

static bool asteroids_errored = false;
static char asteroids_last_error[ASTEROIDS_ERRBUF_SIZE];

const char * asteroids_get_error(void) {
  if (asteroids_errored) {
    return (const char *)&asteroids_last_error;
  } else {
    return (const char *)&"";
  }
}

void asteroids_clear_error(void) {
  asteroids_errored = false;
}

void asteroids_set_error(const char* msg) {
  strncpy(asteroids_last_error, msg, sizeof(asteroids_last_error)-1);
  asteroids_errored = true;
}
