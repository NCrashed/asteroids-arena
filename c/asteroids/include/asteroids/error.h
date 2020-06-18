/**
  Module that handles global error message that was occured at last executed
  asteroid function. Used to print message to user when some function fails.
*/
#ifndef ASTEROIDS_ERROR_H
#define ASTEROIDS_ERROR_H

/// Maximum amount of entities in game possible
#define ASTEROIDS_ERRBUF_SIZE 1024

/// Get last occured error message
const char * asteroids_get_error(void);

/// Clear last occured error flag
void asteroids_clear_error(void);

/// Set last error flag to true and copy error message to last error buffer
void asteroids_set_error(const char* msg);

#endif /* ASTEROIDS_ERROR_H */
