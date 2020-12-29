{ stdenv, fetchFromGitHub, cmake }:

stdenv.mkDerivation rec {
  pname = "entt";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "skypjack";
    repo = "entt";
    rev = "4b38d81f0084b41237238cfa244b5aa4e018811b";
    sha256 = "055z5yz5m9d1a5qbmlawp8bb2cf1md7w31jjsgz7a8p7d0jvhfp7";
  };
  nativeBuildInputs = [ cmake ];
  buildInputs = [ ];

  /* installPhase = ''
    mkdir -p $out/bin
    cp asteroids $out/bin
  ''; */

  meta = with stdenv.lib; {
    description = "Header-only, tiny and easy to use library for game programming";
    longDescription = ''
    EnTT is a header-only, tiny and easy to use library for game programming and much more written in modern C++, mainly known for its innovative entity-component-system (ECS) model.
    Among others, it's used in Minecraft and Minecraft Earth by Mojang, and the ArcGIS Runtime SDKs by Esri.
    '';
    homepage = "https://github.com/skypjack/entt";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
