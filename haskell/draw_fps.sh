set -xe
cabal new-run plotting -- single fps.out
xdg-open fps.png
