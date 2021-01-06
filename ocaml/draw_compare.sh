set -xe
../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps multiple OCaml ./fps.out C ../c/asteroids/fps.out
mv fps_many.png fps_compare.png
xdg-open fps_compare.png
