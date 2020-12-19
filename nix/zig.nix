{ stdenv, fetchFromGitHub, cmake, llvmPackages_11, libxml2, zlib, substituteAll }:

llvmPackages_11.stdenv.mkDerivation rec {
  version = "0.7.1";
  pname = "zig";

  src = fetchFromGitHub {
    owner = "ziglang";
    repo = pname;
    rev = "8591f30b0d53a597682bebdfcd570f5f44339b26";
    sha256 = "0gk6pmbr89jf57jyvyc49hlb597vvrwpbnld1vb2pjv9d73s03r3";
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [
    llvmPackages_11.clang-unwrapped
    llvmPackages_11.llvm
    llvmPackages_11.lld
    libxml2
    zlib
  ];

  preBuild = ''
    export HOME=$TMPDIR;
  '';

  checkPhase = ''
    runHook preCheck
    cp -r $src/test ./test
    chmod -R u+rw ./test
    ./zig test -lc -I ./test ./test/stage1/behavior.zig
    runHook postCheck
  '';

  doCheck = true;

  meta = with stdenv.lib; {
    description =
      "General-purpose programming language and toolchain for maintaining robust, optimal, and reusable software";
    homepage = "https://ziglang.org/";
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ maintainers.andrewrk ];
  };
}
