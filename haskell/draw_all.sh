cabal new-run plotting -- multiple C ../c/asteroids/fps.out Rust ../rust/asteroids/fps.out Zig ../zig/asteroids/fps.out "D DMD" ../d/asteroids/fps.out_dmd "D LDC" ../d/asteroids/fps.out_ldc_pgo
mv fps_many.png ..
xdg-open ../fps_many.png
