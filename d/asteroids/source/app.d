import bindbc.sdl;
import core.time;
import std.stdio;

import asteroids.world;
import asteroids.input;

/// Iterate through user input and window events
bool process_events(ref InputEvents events, ref WorldSize world_size) {
	SDL_Event event = void;
	while (SDL_PollEvent(&event) != 0) {
		switch (event.type) {
			case SDL_QUIT: return true;
			case SDL_KEYDOWN: switch (event.key.keysym.sym) {
				case SDLK_ESCAPE: return true;
				case SDLK_UP: events.ship_thrust = true; break;
				case SDLK_LEFT: events.ship_left = true; break;
				case SDLK_RIGHT: events.ship_right = true; break;
				case SDLK_SPACE: events.ship_fire = true; break;
				default: {}
			} break;
			case SDL_KEYUP: switch (event.key.keysym.sym) {
				case SDLK_UP: events.ship_thrust = false; break;
				case SDLK_LEFT: events.ship_left = false; break;
				case SDLK_RIGHT: events.ship_right = false; break;
				case SDLK_SPACE: events.ship_fire = false; break;
				default: {}
			} break;
			case SDL_WINDOWEVENT: switch (event.window.event) {
				case SDL_WINDOWEVENT_RESIZED:
					world_size.width = event.window.data1;
					world_size.height = event.window.data2;
					break;
				case SDL_WINDOWEVENT_SIZE_CHANGED:
					world_size.width = event.window.data1;
					world_size.height = event.window.data2;
					break;
				default: {}
			} break;
			default: {}
		}
	}

	return false;
}

void main()
{
	if (SDL_Init(SDL_INIT_EVERYTHING)) {
		SDL_Log("SDL init error: %s\n", SDL_GetError());
		return;
	}
	scope(exit) SDL_Quit();

	immutable flags = 0;
	immutable initted = Mix_Init(flags);
	if ((initted & flags) != flags) {
		SDL_Log("Unable to initialize SDL Mixer: %s\n", SDL_GetError());
		return;
	}
	scope(exit) Mix_Quit();

	immutable audio_init = Mix_OpenAudio(11025, AUDIO_S16SYS, 2, 1024);
	if (audio_init < 0) {
		SDL_Log("Unable to create audio device: %s\n", Mix_GetError());
		return;
	}
	scope(exit) Mix_CloseAudio();

	auto screen = SDL_CreateWindow("Asteroids", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, initial_width, initial_height, SDL_WINDOW_OPENGL);
	if (!screen) {
		SDL_Log("SDL window creation error: %s\n", SDL_GetError());
		return;
	}
	scope(exit) SDL_DestroyWindow(screen);

	auto renderer = SDL_CreateRenderer(screen, -1, SDL_RENDERER_ACCELERATED);
	if (!renderer) {
		SDL_Log("Unable to create renderer: %s\n", SDL_GetError());
		return;
	}
	scope(exit) SDL_DestroyRenderer(renderer);

	// Here we define which components are supported by the world
	auto w = new World!(
			WorldSize,
			Position,
			Velocity,
			Rotation,
			Radius,
			Mass,
			Player,
		)("./sounds");

	auto fps_file = File("fps.out", "w");
	auto i = 1;
	auto quit = false;
	auto input_events = InputEvents();
	while (!quit) {
		immutable t1 = MonoTime.currTime();
		quit = process_events(input_events, w.worldSize.global);

		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
		SDL_RenderClear(renderer);
		SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
		w.render(renderer);
		SDL_RenderPresent(renderer);

		immutable t2 = MonoTime.currTime();
		immutable dt = cast(float)(t2 - t1).total!"usecs"() / 1000_000;
		w.step(dt, input_events);
		w.maintain();
	
		immutable fps = 1 / dt;
		i += 1;
		if (i % 1000 == 0) {
			SDL_Log("%f", fps);
			fps_file.writeln(i,",",fps);
		}
	}
}