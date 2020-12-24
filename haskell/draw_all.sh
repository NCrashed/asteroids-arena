cabal new-run plotting -- multiple C ../c/asteroids/fps.out Rust ../rust/asteroids/fps.out Zig ../zig/asteroids/fps.out D ../d/asteroids/fps.out
mv fps_many.png ..
xdg-open ../fps_many.png
