zig build
valgrind --tool=callgrind zig-cache/bin/asteroids
kcachegrind callgrind.out*
