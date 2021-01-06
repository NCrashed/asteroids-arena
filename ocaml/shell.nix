with import ../nix/pkgs.nix {
    config = {
      packageOverrides = pkgs: {

      };
    };
  };
let ocaml = ocaml-ng.ocamlPackages.ocaml;
    opam2nix = import ./opam2nix.nix {};
    selection = opam2nix.build {
      inherit ocaml;
      selection = ./opam-selection.nix;
      src = ./.;
    };
in stdenv.mkDerivation rec {
  name = "asteroids-ocaml-env";
  env = buildEnv { name = name; paths = buildInputs; };

  CAML_LD_LIBRARY_PATH="${SDL2}/lib:${SDL2_mixer}/lib";
  shellHook = ''
    eval $(opam env)
  '';
  buildInputs = [
    SDL2
    SDL2_mixer
    m4
    opam
    ocamlPackages.findlib
    valgrind
    kdeApplications.kcachegrind
    pkg-config
  ];
}
