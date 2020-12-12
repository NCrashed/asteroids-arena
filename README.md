# asteroids-arena

The repo contains demo implementations of simple game in several languages to test their average performance, expressive power and my subjective
joy of their usage.

# Game concept

The game is 2D. The world is wrapped around edges. You are a triangular space ship with single engine without breaks. The ship has single gun that fires bullets.
The world is filled with circular asteroids that doesn't collide with each other and collide with you. On collision with the asteroid
you die. Asteroid breaks into two smaller ones on collision with bullet, but if asteroid is smaller a threshold it is destroyed. Bullets
don't collide with the ship. Asteroids are randomly distributed across world on start. You win if you kill all asteroids, you loose when
you die.

# Implementation restrictions

* Implement entity-component-system design.
* Separate world simulation from rendering to benchmark TPS between languages.
* Use SDL for rendering.

# Languages

I am personally interested to compare some not common languages for gamedev.

Languages:
- Haskell. My major language. The goal to test pressure of GC on performance and if it really more comfortable for game developing. **Done**.
- C. Control group for performance. **Done**.
- ATS2. Low level FP language with dependent typing that will allow mutation in place, redundant checks elimination and zero cost abstractions. **On hold**.
- D. Curious to test it with GC and without GC. Not started yet.
- Zig. Low level alternative to pure C. Curious in performance and if it easy to bind to C libraries. Not started yet.
- Rust. Curious in performance and expressive power comparing to Haskell. **Done**.

# Results

![Gameplay](./screenshots/c_001.png)

## FPS

FPS plots are built with [haskell utility](./haskell/plotting). Like:
```
cabal new-run plotting -- fps.out
```
In haskell nix-shell.

### Haskell
FPS now stable 40-100, smooth gameplay.
![Haskell FPS](./haskell/fps.png)

### C
There is smooth gameplay. I experienced drops to 500 FPS occasionally.
![C FPS](./c/asteroids/fps.png)

### Rust
Smooth gameplay. I consider the Rust implementation as most satisfying in terms of performance/time spent ratio.
![C FPS](./rust/fps.png)
