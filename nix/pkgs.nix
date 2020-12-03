# To update nix-prefetch-git https://github.com/NixOS/nixpkgs-channels
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs-channels";
  rev = "84d74ae9c9cbed73274b8e4e00be14688ffc93fe";
  sha256  = "0ww70kl08rpcsxb9xdx8m48vz41dpss4hh3vvsmswll35l158x0v";
})
