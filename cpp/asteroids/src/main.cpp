#include "asteroids/game.h"
#include "asteroids/world.h"

#include <SDL2/SDL.h>
#include <SDL_mixer.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <functional>

class scope_guard {
  using exit_func = std::function<void()>;
public:
  scope_guard(exit_func f) : f(f) {}
  ~scope_guard() { if (f) f(); }
private:
  exit_func f;
};

static bool process_events(input_events &events) {
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
    if (e.type == SDL_QUIT) {
      return true;
    }
    else if (e.type == SDL_KEYDOWN) {
      if (e.key.keysym.sym == SDLK_ESCAPE) {
        return true;
      }
      else if (e.key.keysym.sym == SDLK_UP) {
        events.ship_thrust = true;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_LEFT) {
        events.ship_left = true;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_RIGHT) {
        events.ship_right = true;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_SPACE) {
        events.ship_fire = true;
        return false;
      }
    }
    else if (e.type == SDL_KEYUP) {
      if (e.key.keysym.sym == SDLK_UP) {
        events.ship_thrust = false;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_LEFT) {
        events.ship_left = false;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_RIGHT) {
        events.ship_right = false;
        return false;
      }
      else if (e.key.keysym.sym == SDLK_SPACE) {
        events.ship_fire = false;
        return false;
      }
    }
  }
  return false;
}

int main(int argc, char *argv[]) {
  if (SDL_Init(SDL_INIT_EVERYTHING)) {
    SDL_Log("SDL init error: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }
  scope_guard sg1{ [] { SDL_Quit(); } };

  int flags = 0;
  int initted = Mix_Init(flags);
  if ((initted & flags) != flags) {
    SDL_Log("Mix_Init: Failed to init: %s\n", Mix_GetError());
    return EXIT_FAILURE;
  }
  scope_guard sg2{ [] { Mix_Quit(); } };

  if(Mix_OpenAudio(11025, AUDIO_S16SYS, 2, 1024) == -1) {
    SDL_Log("Mix_OpenAudio: %s\n", Mix_GetError());
    return EXIT_FAILURE;
  }
  scope_guard sg3{ [] { Mix_CloseAudio(); } };

  SDL_Window *window = SDL_CreateWindow("Asteroids",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    WORLD_WIDTH, WORLD_HEIGHT, SDL_WINDOW_SHOWN);
  if (!window) {
    SDL_Log("SDL window creation error: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }
  scope_guard sg4{ [window] { SDL_DestroyWindow(window); } };

  SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) {
    SDL_Log("SDL renderer creation error: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }
  scope_guard sg5{ [renderer] { SDL_DestroyRenderer(renderer); } };

  game g{ std::filesystem::current_path() };
  g.start();

  std::ofstream ofile("fps.out");
  if (!ofile) {
    SDL_Log("Failed to open fps.out for writing!");
  }

  using namespace std::chrono;
  int frame = 0;
  auto last_tick = high_resolution_clock::now();
  float dt = 0;
  while (true) {
    auto current_tick = high_resolution_clock::now();

    if (process_events(g.events))
      break;

    g.step(dt);

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    g.render(renderer);
    SDL_RenderPresent(renderer);

    last_tick = high_resolution_clock::now();
    dt = duration_cast<duration<float>>(last_tick - current_tick).count();

    frame++;
    auto fps = 1 / dt;
    if (frame % 2000 == 0) {
      SDL_Log("FPS: %f", fps);
    }

    // skip file print time
    if (frame % 1000 == 0) {
      ofile << frame << "," << fps << "\n";
    }
  }

  return EXIT_SUCCESS;
}
