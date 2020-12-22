MODULES=src/*
CFLAGS=-Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow -Iinclude $(shell pkg-config --cflags sdl2 SDL2_mixer)
LDFLAGS=$(shell pkg-config --libs sdl2 SDL2_mixer) -lm

all:
	gcc -g ${CFLAGS} -o asteroids ${MODULES} ${LDFLAGS}

opt:
	gcc -O3 -ffast-math ${CFLAGS} -o asteroids ${MODULES} ${LDFLAGS}

small:
	gcc -Os ${CFLAGS} -o asteroids ${MODULES} ${LDFLAGS}
	-strip asteroids
	-sstrip asteroids

clean:
	rm asteroids
