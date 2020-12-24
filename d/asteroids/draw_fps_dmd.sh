set -xe
../../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps single ./fps.out_dmd
mv fps.png fps_dmd.png
xdg-open fps_dmd.png
