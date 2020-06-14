# asteroids

Basic haskell implementation of asteroids game for benchmarks and showcase.

* Rendering is SDL based.
* Apecs for entity component system.
* Benchmarks of TPS (tick per second).

# Building

The build scripts are nix based. To build you need simply call:
```
nix-build
```

# Hacking

To enter shell with cabal and ghcid:
```
nix-shell
```

To build and run: 
```
./run.sh
```

To start watching for source file with ghcid:
```
./watch.sh
```
