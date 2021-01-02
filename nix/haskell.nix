{ compiler, useliquid ? false }:
self: super:
let
  ghc-override = import "${import ./project.nix}/lib/ghc-override.nix";
  pkgs = self;
  lib = pkgs.haskell.lib;
in
ghc-override compiler (self: super: lib.packagesFromDirectory { directory = ./pkgs; } self super //
  {
    Judy = pkgs.judy;
    inherit useliquid;
  }) super
