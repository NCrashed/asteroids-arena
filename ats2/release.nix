let
  pkgs = import ../nix/pkgs.nix { inherit config; };
  config = {
    packageOverrides = new: rec {
      ats2 = new.callPackage ./compiler {};
      asteroids-ats2 = new.callPackage ./asteroids {};
    };
    allowBroken = true;
    allowUnfree = true;
  };
in {
  inherit pkgs;
  shell = pkgs.asteroids-ats2;

  packages = {
    inherit (pkgs) asteroids-ats2;
  };
}
