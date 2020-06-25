staload "contrib/SDL2/SATS/SDL.sats"

implement main0 () = () where
{
  var ver: SDL_version
  val () = SDL_VERSION (ver)
  val () = println! ("SDL(VERSION) = ", ver.major, ".", ver.minor, ".", ver.patch)
  val () = SDL_GetVersion (ver)
  val () = println! ("SDL(GetVersion) = ", ver.major, ".", ver.minor, ".", ver.patch)
}
