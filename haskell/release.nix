let
  nixpkgs = import ../nix/pkgs.nix;
  project = import ((nixpkgs {}).fetchFromGitHub {
    owner = "NCrashed";
    repo = "haskell-nix";
    rev = "b1fcade2695a48ef7cff2833b9d0c5662de70dbb";
    sha256  = "1vnbi1g6yswq5vb1fqjmh2mdnzynka2v17rnyh9j170xnza3crg2";
  }) { inherit nixpkgs; };
in project {
  packages = {
    asteroids = ./asteroids;
  };
}
