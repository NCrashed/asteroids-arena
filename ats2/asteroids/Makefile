MODULES=src/*
CFLAGS=-I $(PATSHOME)/contrib $(shell pkg-config --cflags sdl2 SDL2_mixer)
LDFLAGS=$(shell pkg-config --libs sdl2 SDL2_mixer)

all:
	patscc -g "${CFLAGS}" -o asteroids ${MODULES} ${LDFLAGS}

opt:
	patscc -O3 "${CFLAGS}" -o asteroids ${MODULES} ${LDFLAGS}

small:
	patscc -Os "${CFLAGS}" -o asteroids ${MODULES} ${LDFLAGS}
	-strip asteroids
	-sstrip asteroids

clean:
	rm asteroids
