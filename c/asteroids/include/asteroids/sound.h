/**
  Handle sound states and cooldowns.
*/
#ifndef ASTEROIDS_SOUND_H
#define ASTEROIDS_SOUND_H

#include "SDL_mixer.h"

#define CHANNEL_COUNT 3

struct sound_resources {
  Mix_Chunk *bang_medium;
  Mix_Chunk *fire;
  Mix_Chunk *thrust;
  float cooldowns[CHANNEL_COUNT];
};

/// Initialize sound resources. Returns non zero code if failed.
int init_sound_resources(struct sound_resources *resources, const char* path);
/// Deallocate sound resources
void destroy_sound_resources(struct sound_resources *resources);
/// Decrease cooldowns for playing sound again
void update_sound_cooldowns(struct sound_resources *resources, float dt);
/// Play sound at given channel and setup cooldown
void play_sound(struct sound_resources *resources, Mix_Chunk *sound, int channel, float cooldown);

#endif /* ASTEROIDS_SOUND_H */
