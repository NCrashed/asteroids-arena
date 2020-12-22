with import ../nix/pkgs.nix {
    config = {
      packageOverrides = pkgs: {
        dmd = pkgs.callPackage ../nix/dmd {};
        ldc = pkgs.callPackage ../nix/ldc {};
      };
    };
  };

stdenv.mkDerivation rec {
  name = "asteroids-d-env";
  env = buildEnv { name = name; paths = buildInputs; };

  buildInputs = [
    dmd
    ldc
    dub
    SDL2
    SDL2_mixer
    valgrind
    kdeApplications.kcachegrind
    pkg-config
  ];
}
