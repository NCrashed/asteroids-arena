# To update nix-prefetch-git https://github.com/NixOS/nixpkgs
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "aa2c22d53f30c37a83d0630cbfc01d247a427cc9";
  sha256  = "0021f0s414s7wffdyjkqjcw23s5yyrr0n76p4bxpp9xk3bblvbhv";
})
