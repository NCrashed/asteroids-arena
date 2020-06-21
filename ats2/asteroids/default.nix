{ stdenv, ats2, pkg-config, SDL2, SDL2_mixer, elfkickers }:

stdenv.mkDerivation rec {
  pname = "asteroids-ats2";
  version = "0.1";

  src = ./.;
  nativeBuildInputs = [ ats2 pkg-config elfkickers ];
  buildInputs = [ SDL2 SDL2_mixer ];

  installPhase = ''
    mkdir -p $out/bin
    cp asteroids $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Clone of asteroids game";
    longDescription = ''
      Minimal clone of original acrade game where you play as a space ship in wrapped
      2D cosmos filled with asteroids. Your task is survive and destroy all asteroids.
    '';
    homepage = "https://github.com/NCrashed/asteroids-arena";
    changelog = "https://github.com/NCrashed/asteroids-arena/ats2/asteroids/CHANGELOG.md";
    license = licenses.mit;
    maintainers = [ "ncrashed@protonmail.com" ];
    platforms = platforms.all;
  };
}
