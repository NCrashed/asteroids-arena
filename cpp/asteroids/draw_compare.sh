set -xe
../../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps multiple "-O3" ./fps.out "-O3 -ffast-math -fno-math-errno -march=native" ./fps.out_all
xdg-open fps_many.png
