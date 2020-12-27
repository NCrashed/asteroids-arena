module asteroids.component.audio;

import decs.storage.global;
import bindbc.sdl;
import std.array;
import std.exception;
import std.path;
import std.stdio;
import std.string;

/// Sound tag, all known sounds by the game
enum Sound: size_t {
  bang = 0,
  fire = 1,
  thrust = 2,
}

/// Count of sounds statically known
enum size_t soundsCount = Sound.max + 1;

/// Get unique cooldown time for a sound
float soundCooldown(Sound s) {
  final switch(s) {
    case Sound.bang: return 0.4;
    case Sound.fire: return 0.2;
    case Sound.thrust: return 0.2;
  }
}

/// Get unique filename for a sound relative to sounds dir
string soundFile(Sound s) {
  final switch(s) {
    case Sound.bang: return "bangMedium.wav";
    case Sound.fire: return "fire.wav";
    case Sound.thrust: return "thrust.wav";
  }
}

/// Global component that holds audio resources in memory
struct Audio {
  Mix_Chunk*[soundsCount] sounds;
  float[soundsCount] cooldowns;

  /// Name of component
  enum name = "audio";
  /// We store size as global value
  alias Storage = GlobalStorage!Audio;

  /// Update cooldown for sound channels
  void updateCooldowns(float dt) {
    foreach(ref cd; cooldowns) {
      if(cd > 0) {
        cd -= dt;
      }
    }
  }

  /// Play given sound if cooldown is over
  void play(Sound sound) {
    if (cooldowns[sound] <= 0) {
      Mix_PlayChannel(-1, sounds[sound], 0);
      cooldowns[sound] = soundCooldown(sound);
    }
  }

  /// Load audio files. After that you can play sounds.
  void loadFiles(string dir) {
    cooldowns[] = 0;

    Mix_AllocateChannels(soundsCount);
    Mix_Volume(-1, 10);

    foreach(i, ref chunk; sounds) {
      chunk = loadWav(chainPath(dir, soundFile(cast(Sound)i)).array);
    }
  }

  /// Load wav file from given directory, caller owns the result.
  private Mix_Chunk* loadWav(string path) {
    writeln("Loading " ~ path);
    auto res = Mix_LoadWAV(path.toStringz);
    if (!res) {
      SDL_Log("%s", SDL_GetError());
      enforce(false, "Failed to load WAV " ~ path);
    }
    return res;
  }
}
