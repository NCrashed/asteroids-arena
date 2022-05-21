{ mkDerivation, base, bytestring, ghc-prim, hspec, Judy, lib
, QuickCheck
}:
mkDerivation {
  pname = "judy";
  version = "0.4.1";
  sha256 = "553bc34f177d206646f898f875a239de494ce76d0c08c42593c68072ec39d546";
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  librarySystemDepends = [ Judy ];
  testHaskellDepends = [ base hspec QuickCheck ];
  homepage = "http://github.com/mwotton/judy";
  description = "Fast, scalable, mutable dynamic arrays, maps and hashes";
  license = lib.licenses.bsd3;
}
