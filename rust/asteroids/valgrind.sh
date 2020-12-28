set -xe
cargo build --release
valgrind --tool=callgrind target/release/asteroids
kcachegrind callgrind.out*
