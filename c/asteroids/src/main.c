#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <SDL2/SDL.h>
#include "SDL_mixer.h"

#include "asteroids/error.h"
#include "asteroids/sound.h"
#include "asteroids/world.h"
#include "asteroids/render.h"

bool processEvents(struct input_events *events) {
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
    if (e.type == SDL_QUIT) {
      return true;
    } else if (e.type == SDL_KEYDOWN) {
      if (e.key.keysym.sym == SDLK_ESCAPE) {
        return true;
      } else if (e.key.keysym.sym == SDLK_UP) {
        events->ship_thrust = true;
        return false;
      } else if (e.key.keysym.sym == SDLK_LEFT) {
        events->ship_left = true;
        return false;
      } else if (e.key.keysym.sym == SDLK_RIGHT) {
        events->ship_right = true;
        return false;
      } else if (e.key.keysym.sym == SDLK_SPACE) {
        events->ship_fire = true;
        return false;
      }
    } else if (e.type == SDL_KEYUP) {
      if (e.key.keysym.sym == SDLK_UP) {
        events->ship_thrust = false;
        return false;
      } else if (e.key.keysym.sym == SDLK_LEFT) {
        events->ship_left = false;
        return false;
      } else if (e.key.keysym.sym == SDLK_RIGHT) {
        events->ship_right = false;
        return false;
      } else if (e.key.keysym.sym == SDLK_SPACE) {
        events->ship_fire = false;
        return false;
      }
    }
  }
  return false;
}

int main(int argc, char *argv[])
{
  if (SDL_Init(SDL_INIT_EVERYTHING)) {
    SDL_Log("SDL init error: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }

  int flags = 0;
  int initted = Mix_Init(flags);
  if ((initted & flags) != flags) {
    SDL_Log("Mix_Init: Failed to init!\n");
    SDL_Log("Mix_Init: %s\n", Mix_GetError());
    SDL_Quit();
    return EXIT_FAILURE;
  }
  if(Mix_OpenAudio(11025, AUDIO_S16SYS, 2, 1024)==-1) {
    SDL_Log("Mix_OpenAudio: %s\n", Mix_GetError());
    Mix_Quit();
    SDL_Quit();
    return EXIT_FAILURE;
  }

  struct sound_resources reses;
  if(init_sound_resources(&reses, "./sounds")) {
    SDL_Log("Failed to load sound resources: %s\n", Mix_GetError());
    Mix_CloseAudio();
    Mix_Quit();
    SDL_Quit();
    return EXIT_FAILURE;
  }

  SDL_Window *window = SDL_CreateWindow("Asteroids",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    WORLD_WIDTH, WORLD_HEIGHT, SDL_WINDOW_SHOWN);
  if (!window) {
    SDL_Log("SDL window creation error: %s\n", SDL_GetError());
    destroy_sound_resources(&reses);
    Mix_CloseAudio();
    Mix_Quit();
    SDL_Quit();
    return EXIT_FAILURE;
  }

  SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) {
    SDL_Log("SDL renderer creation error: %s\n", SDL_GetError());
    SDL_DestroyWindow(window);
    destroy_sound_resources(&reses);
    Mix_CloseAudio();
    Mix_Quit();
    SDL_Quit();
    return EXIT_FAILURE;
  }

  struct World world;
  if (init_world(&world, &reses)) {
    SDL_Log("World allocation error: %s\n", asteroids_get_error());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    destroy_sound_resources(&reses);
    Mix_CloseAudio();
    Mix_Quit();
    SDL_Quit();
    return EXIT_FAILURE;
  }

  struct input_events events;
  init_input_events(&events);

  float dt = 0;
  unsigned int lastTick = 0;
  int i = 0;
  while(true) {
    bool quit = processEvents(&events);
    if (quit) break;

    unsigned int currentTick = SDL_GetTicks();
    dt = (float)(currentTick - lastTick) / 1000.0;
    lastTick = currentTick;

    update_sound_cooldowns(&reses, dt);
    if (step_world(&world, dt, &events)) {
      SDL_Log("World stepping error: %s\n", asteroids_get_error());
      break;
    }

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    for (size_t e=0; e < world.entity_counter; e++) {
      render_entity(renderer, e, &world);
    }
    SDL_RenderPresent(renderer);
    i += 1;
    if (i % 20000 == 0) {
      SDL_Log("FPS: %f", 1 / dt);
      i = 0;
    }
  }

  destroy_world(&world);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  destroy_sound_resources(&reses);
  Mix_CloseAudio();
  Mix_Quit();
  SDL_Quit();
  return EXIT_SUCCESS;
}
