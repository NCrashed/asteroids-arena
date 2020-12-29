set -xe
../../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps multiple "-O3 -fno-math-errno" ./fps.out_math_errno "-O3 -ffast-math -march=native" ./fps.out_ffast_math_native
xdg-open fps_many.png
