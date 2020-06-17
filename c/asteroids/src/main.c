#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

#include "asteroids/error.h"
#include "asteroids/world.h"

bool processEvents(void) {
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
    if (e.type == SDL_QUIT) {
      return true;
    } else if (e.type == SDL_KEYDOWN) {
      if (e.key.keysym.sym == SDLK_ESCAPE) {
        return true;
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

  SDL_Window *window = SDL_CreateWindow("Asteroids",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    1480, 1024, SDL_WINDOW_SHOWN);
  if (!window) {
    SDL_Log("SDL window creation error: %s\n", SDL_GetError());
    SDL_Quit();
    return EXIT_FAILURE;
  }

  SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) {
    SDL_Log("SDL renderer creation error: %s\n", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_FAILURE;
  }

  struct World world;
  if (init_world(&world)) {
    SDL_Log("World allocation error: %s\n", asteroids_get_error());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_FAILURE;
  }

  while(true) {
    bool quit = processEvents();
    if (quit) break;

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderPresent(renderer);
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return EXIT_SUCCESS;
}
