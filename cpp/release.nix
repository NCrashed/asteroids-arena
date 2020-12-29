let
  pkgs = import ../nix/pkgs.nix { inherit config; };
  config = {
    packageOverrides = new: rec {
      asteroids = new.callPackage ./asteroids {};
      entt = new.callPackage ./pkgs/entt.nix {};
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
