with import ../nix/pkgs.nix {
    config = {
      packageOverrides = pkgs: {
        zig = pkgs.callPackage ../nix/zig.nix {};
      };
    };
  };

stdenv.mkDerivation rec {
  name = "asteroids-zig-env";
  env = buildEnv { name = name; paths = buildInputs; };

  buildInputs = [
    zig
    SDL2
    SDL2_mixer
    valgrind
    kdeApplications.kcachegrind
    pkg-config
  ];
}
