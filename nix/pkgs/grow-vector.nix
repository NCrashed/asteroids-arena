{ mkDerivation, base, primitive, quickcheck-instances, stdenv
, tasty, tasty-discover, tasty-hspec, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "grow-vector";
  version = "0.1.0.0";
  sha256 = "9d4c1106e8e1ba062f0c29d3f2571ae4e547a8db5ce064b306d5eb2780fa54ad";
  libraryHaskellDepends = [ base primitive vector ];
  testHaskellDepends = [
    base primitive quickcheck-instances tasty tasty-discover
    tasty-hspec tasty-quickcheck
  ];
  testToolDepends = [ tasty-discover ];
  description = "Mutable vector with efficient appends";
  license = stdenv.lib.licenses.mit;
}
