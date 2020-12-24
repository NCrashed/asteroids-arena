set -xe
../../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps single ./fps.out_ldc
mv fps.png fps_ldc.png
xdg-open fps_ldc.png
