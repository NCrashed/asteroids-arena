#include "asteroids/sound.h"

#include <SDL_mixer.h>

sound_resource::sound_resource(const std::filesystem::path &p)
  : r(Mix_LoadWAV((const char *)p.u8string().c_str()), Mix_FreeChunk) {
  if (!r)
    throw std::runtime_error("cannot load: " + p.string());
}

sound_resources::sound_resources(const std::filesystem::path &p)
  : bang_medium(p / "bangMedium.wav")
  , fire(p / "fire.wav")
  , thrust(p / "thrust.wav") {
  Mix_AllocateChannels(channel_count);
  Mix_Volume(-1, 10);
}

void sound_resources::update_sound_cooldowns(float dt) {
  for (auto &cd : cooldowns) {
    if (cd > 0) {
      cd -= dt;
    }
  }
}

void sound_resources::play_sound(const sound_resource &sound, int channel, float cooldown) {
  if (channel < 0 || channel >= channel_count || cooldowns[channel] > 0) return;
  cooldowns[channel] = cooldown;
  Mix_PlayChannel(channel, sound.r.get(), 0);
}
