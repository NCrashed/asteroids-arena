set -xe
RUSTFLAGS="-Cprofile-generate=profile.raw" cargo run --release
