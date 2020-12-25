ldc-profdata merge -output=profile.data profile.raw
DFLAGS="-fprofile-instr-use=profile.data -flto=full -defaultlib=phobos2-ldc-lto,druntime-ldc-lto" dub run --build=release-nobounds --compiler=ldmd2
