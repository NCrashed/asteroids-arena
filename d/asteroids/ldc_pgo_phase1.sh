DFLAGS="-fprofile-instr-generate=profile.raw -flto=full -defaultlib=phobos2-ldc-lto,druntime-ldc-lto" dub run --build=release-nobounds --compiler=ldmd2
