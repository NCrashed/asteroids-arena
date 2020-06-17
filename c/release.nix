let
  pkgs = import ../nix/pkgs.nix { inherit config; };
  config = {
    packageOverrides = new: rec {
      asteroids = new.callPackage ./asteroids {};
    };
    allowBroken = true;
    allowUnfree = true;
  };
in {
  inherit pkgs;
  shell = pkgs.asteroids;

  packages = {
    inherit (pkgs) asteroids;
  };
}
