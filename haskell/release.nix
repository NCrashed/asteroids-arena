let
  nixpkgs = import ../nix/pkgs.nix;
  project = import (import ../nix/project.nix) { inherit nixpkgs; };
in project rec {
  packages = {
    asteroids = ./asteroids;
    plotting = ./plotting;
  };
  shellHook = pkgs: ''
    ${pkgs.addLiquidSolverHook}
  '';
  shellTools = pkgs: with pkgs.haskellPackages; [
    cabal-install ghcid
    pkgs.cloc
  ];
  overlays = [
      (import (import ../nix/liquidhaskell.nix) { inherit compiler; })
      (import ../nix/haskell.nix { inherit compiler; })
      (import ../nix/cloc-overlay.nix)
    ];
  overlaysAfter = [ ((import ./solver.nix) { inherit compiler; }) ];
  compiler = "ghc8102";
}
