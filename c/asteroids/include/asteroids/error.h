/**
  Module that defines whole game state and storages for different components.
*/
#ifndef ASTEROIDS_ERROR_H
#define ASTEROIDS_ERROR_H

#include <string.h>

#define ASTEROIDS_ERRBUF_SIZE 1024

static bool asteroids_errored = false;
static char asteroids_last_error[ASTEROIDS_ERRBUF_SIZE];

/// Get last occured error message
const char * asteroids_get_error(void) {
  if (asteroids_errored) {
    return (const char *)&asteroids_last_error;
  } else {
    return (const char *)&"";
  }
}

/// Clear last occured error flag
void asteroids_clear_error(void) {
  asteroids_errored = false;
}

/// Set last error flag to true and copy error message to last error buffer
void asteroids_set_error(const char* msg) {
  strncpy(asteroids_last_error, msg, sizeof(asteroids_last_error)-1);
  asteroids_errored = true;
}

#endif /* ASTEROIDS_ERROR_H */
