set -xe
../../haskell/dist-newstyle/build/x86_64-linux/ghc-8.10.2/plotting-0.1.0.0/x/calc-fps/build/calc-fps/calc-fps multiple legion ./fps.out_legion specs ./fps.out_specs
mv fps_many.png ecs_compare.png
xdg-open ecs_compare.png
