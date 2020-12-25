{ stdenv, fetchFromGitHub, makeWrapper, perlPackages }:

stdenv.mkDerivation rec {
  pname = "cloc";
  version = "1.89";

  src = fetchFromGitHub {
    owner = "AlDanial";
    repo = "cloc";
    rev = "325b9d74e3e4f6034c56c329b77fedbae47d91a2";
    sha256 = "1gywg91v4cl1a50ksn9wr20apccmccbc1c5vrbkpgs0qar3xni1p";
  };

  setSourceRoot = ''
    sourceRoot=$(echo */Unix)
  '';

  buildInputs = [ makeWrapper ] ++ (with perlPackages; [
    perl AlgorithmDiff ParallelForkManager RegexpCommon
  ]);

  makeFlags = [ "prefix=" "DESTDIR=$(out)" "INSTALL=install" ];

  postFixup = "wrapProgram $out/bin/cloc --prefix PERL5LIB : $PERL5LIB";

  meta = {
    description = "A program that counts lines of source code";
    homepage = "https://github.com/AlDanial/cloc";
    license = stdenv.lib.licenses.gpl2;
    platforms = stdenv.lib.platforms.all;
    maintainers = with stdenv.lib.maintainers; [ rycee ];
  };
}
