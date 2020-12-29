set -xe
make opt
valgrind --tool=callgrind ./asteroids
kcachegrind callgrind.out*
