let
  nixpkgs = import ../nix/pkgs.nix;
  project = import ((nixpkgs {}).fetchFromGitHub {
    owner = "NCrashed";
    repo = "haskell-nix";
    rev = "0c1c27a22daa78d359d7704448e4c6e2512cde5d";
    sha256  = "1vnbi1g6ydwq5vb1fqjmh2mdnzynka2v17rnyh9j170xnza3crg2";
  }) { inherit nixpkgs; };
in project {
  packages = {
    asteroids = ./asteroids;
    plotting = ./plotting;
  };
  compiler = "ghc8101";
}
