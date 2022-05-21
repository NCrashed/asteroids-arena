{ liquidhaskell ? false }:
let
  nixpkgs = import ../nix/pkgs.nix;
  project = import (import ../nix/project.nix) { inherit nixpkgs; };
in project rec {
  packages = {
    asteroids = ./asteroids;
    plotting = ./plotting;
  };
  shellHook = pkgs: ''
    ${if liquidhaskell then pkgs.addLiquidSolverHook else ""}
    export PKG_CONFIG_PATH=${pkgs.SDL2.dev}/lib/pkgconfig:${pkgs.SDL2_mixer}/lib/pkgconfig
    export C_INCLUDE_PATH=${pkgs.SDL2.dev}/include:${pkgs.SDL2_mixer}/include
    export LIBRARY_PATH=${pkgs.SDL2}/lib:${pkgs.SDL2_mixer}/lib
  '';
  shellTools = pkgs: with pkgs.haskellPackages; [
    cabal-install ghcid
    pkgs.cloc
  ];
  overlays = (if liquidhaskell then [
      (import (import ../nix/liquidhaskell.nix) { inherit compiler; })
    ] else [])
    ++
    [
      (import ../nix/haskell.nix { inherit compiler; useliquid = liquidhaskell; })
      (import ../nix/cloc-overlay.nix)
    ];
  overlaysAfter = if liquidhaskell then [ ((import ./solver.nix) { inherit compiler; }) ] else [];
  compiler = "ghc8107";
}
