{ compiler }:
self: super:
let
  ghc-override = import "${import ../nix/project.nix}/lib/ghc-override.nix";
  pkgs = self;
in
ghc-override compiler ( self: super: {
  asteroids = pkgs.addLiquidSolver super.asteroids;
}) super
