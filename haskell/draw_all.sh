cabal new-run plotting -- multiple Haskell ./fps.out C ../c/asteroids/fps.out Rust ../rust/asteroids/fps.out Zig ../zig/asteroids/fps.out
mv fps_many.png ..
xdg-open ../fps_many.png
