#include "asteroids/sound.h"
#include <SDL2/SDL.h>

int init_sound_resources(struct sound_resources *resources, const char* path)
{
  Mix_AllocateChannels(CHANNEL_COUNT);
  Mix_Volume(-1, 10);
  char buff[1024];
  memset(resources, 0, sizeof(struct sound_resources));

  sprintf(buff, "%s/bangMedium.wav", path);
  SDL_Log("Loading %s\n", buff);
  resources->bang_medium = Mix_LoadWAV(buff);
  if (!resources->bang_medium) {
    destroy_sound_resources(resources);
    return 1;
  }

  sprintf(buff, "%s/fire.wav", path);
  SDL_Log("Loading %s\n", buff);
  resources->fire = Mix_LoadWAV(buff);
  if (!resources->fire) {
    destroy_sound_resources(resources);
    return 1;
  }

  sprintf(buff, "%s/thrust.wav", path);
  SDL_Log("Loading %s\n", buff);
  resources->thrust = Mix_LoadWAV(buff);
  if (!resources->thrust) {
    destroy_sound_resources(resources);
    return 1;
  }

  return 0;
}

void destroy_sound_resources(struct sound_resources *resources)
{
  if (resources->bang_medium) Mix_FreeChunk(resources->bang_medium);
  if (resources->fire) Mix_FreeChunk(resources->fire);
  if (resources->thrust) Mix_FreeChunk(resources->thrust);
  memset(resources, 0, sizeof(struct sound_resources));
}

void update_sound_cooldowns(struct sound_resources *resources, float dt) {
  for (size_t i=0; i < CHANNEL_COUNT; i++) {
    if (resources->cooldowns[i] > 0) {
      resources->cooldowns[i] -= dt;
    }
  }
}

void play_sound(struct sound_resources *resources, Mix_Chunk *sound, int channel, float cooldown) {
  if (channel < 0 || channel >= CHANNEL_COUNT || resources->cooldowns[channel] > 0) return;
  resources->cooldowns[channel] = cooldown;
  Mix_PlayChannel(channel, sound, 0);
}
