
{ stdenv, fetchurl, gmp
, withEmacsSupport ? true
, withContrib ? true }:

let
  versionPkg = "0.4.0" ;

  contrib = fetchurl {
    url = "https://sourceforge.net/projects/ats2-lang/files/ats2-lang/ats2-postiats-${versionPkg}/ATS2-Postiats-contrib-${versionPkg}.tgz/download?use_mirror=liquidtelecom";
    sha256 = "1fckpk62w2ifbdp4r4gjk4df02nv7dqqpc449y8a8wv4vl8ilr7m";
  };

  postInstallContrib = stdenv.lib.optionalString withContrib
  ''
    local contribDir=$out/lib/ats2-postiats-*/ ;
    mkdir -p $contribDir ;
    tar -xzf "${contrib}" --strip-components 1 -C $contribDir ;
  '';

  postInstallEmacs = stdenv.lib.optionalString withEmacsSupport
  ''
    local siteLispDir=$out/share/emacs/site-lisp/ats2 ;
    mkdir -p $siteLispDir ;
    install -m 0644 -v ./utils/emacs/*.el $siteLispDir ;
  '';
in

stdenv.mkDerivation rec {
  pname = "ats2";
  version = versionPkg;

  src = fetchurl {
    url = "https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-${version}/ATS2-Postiats-${version}.tgz";
    sha256 = "1h39113rfxwxhdwvadrdfz23h9hs7rsg2xa063n4bnly88nvcjd7";
  };

  buildInputs = [ gmp ];

  setupHook = with stdenv.lib;
    let
      hookFiles =
        [ ./setup-hook.sh ]
        ++ optional withContrib ./setup-contrib-hook.sh;
    in
      builtins.toFile "setupHook.sh"
      (concatMapStringsSep "\n" builtins.readFile hookFiles);

  postInstall = postInstallContrib + postInstallEmacs;

  meta = with stdenv.lib; {
    description = "Functional programming language with dependent types";
    homepage    = "http://www.ats-lang.org";
    license     = licenses.gpl3Plus;
    platforms   = platforms.linux;
    maintainers = with maintainers; [ thoughtpolice ttuegel bbarker ];
  };
}
