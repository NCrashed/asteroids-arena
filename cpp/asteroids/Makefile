MODULES=src/*
CPPFLAGS=-std=c++2a -Iinclude $(shell pkg-config --cflags sdl2 SDL2_mixer)
LDFLAGS=$(shell pkg-config --libs sdl2 SDL2_mixer) -lm

all:
	g++ -g ${CPPFLAGS} -o asteroids ${MODULES} ${LDFLAGS}

opt:
	g++ -O3 ${CPPFLAGS} -o asteroids ${MODULES} ${LDFLAGS}

small:
	g++ -Os ${CPPFLAGS} -o asteroids ${MODULES} ${LDFLAGS}
	-strip asteroids
	-sstrip asteroids

clean:
	rm asteroids
