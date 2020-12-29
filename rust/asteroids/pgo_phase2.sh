set -xe
llvm-profdata merge -o profile.merged profile.raw
RUSTFLAGS="-Cprofile-use=$(pwd)/profile.merged" cargo run --release
