zig build
valgrind --tool=callgrind ./asteroids
kcachegrind callgrind.out*
