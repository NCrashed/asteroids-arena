{ compiler }:
self: super:
let
  ghc-override = import "${import ./project.nix}/lib/ghc-override.nix";
  pkgs = self;
  lib = pkgs.haskell.lib;
in
ghc-override compiler (lib.packagesFromDirectory { directory = ./pkgs; }) super
