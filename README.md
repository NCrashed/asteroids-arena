# asteroids-arena

The repo contains demo implementations of simple game in several languages to test their average performance, expressive power and my subjective
joy of their usage.

# Game concept

The game is 2D. The world is wrapped around edges. You are a triangular space ship with single engine without breaks. The ship has single gun that fires bullets.
The world is filled with circular asteroids that doesn't collide with each other and collide with you. On collision with the asteroid
you die. Asteroid breaks into two smaller ones on collision with bullet, but if asteroid is smaller a threshold it is destroyed. Bullets
don't collide with the ship. Asteroids are randomly distributed across world on start. You win if you kill all asteroids, you loose when
you die.

Important gameplay feature that bullets has relative speed to the ship when spawned, so if you shoot backward, you can get bullet that doesn't move relative to
the world. There should be three supported sound effects. First, thrust sound when ship accelerates. Second, fire sound. Finally, bangs on asteroid destruction and player death. For simplicity player respawns in center of world on death.

# Implementation restrictions

* Implement entity-component-system design. If language design is not suited well for the pattern, it is acceptable to choose another design.
* Asteroids have [circloid shape](./c/asteroids/src/render.c#L35) and you should not memorize asteroid lines. That restriction allows to see how language backend
deals with heavy load of 2D rendering (trigonometry e.x.). But you can batch all rendering calls.
* Measure FPS of full frame with world step and rendering.
* Use SDL for rendering.
* Concurrency is allowed.

# Languages

I am personally interested to compare some not common languages for gamedev.

Languages:
- Haskell. My major language. The goal to test pressure of GC on performance and if it really more comfortable for game developing. **Done**.
- C. Control group for performance. **Done**.
- ATS2. Low level FP language with dependent typing that will allow mutation in place, redundant checks elimination and zero cost abstractions. **On hold**.
- D. Curious to test it with GC and without GC. Use custom library with meta programming for boilerplate generation **Done**.
- Zig. Low level alternative to pure C. Curious in performance and if it easy to bind to C libraries. **Done**.
- Rust. Curious in performance and expressive power comparing to Haskell. **Done**.
- С++. Widely used in game development. First implementation is unoptimized translation of C version with [entt](https://github.com/skypjack/entt) library **Done**.
- OCaml. FP language with C backend, should be fast enough and with FP expression power. **In Progress**. Current implementation is not optimized.

# Contributors

- [@egorpugin](https://github.com/egorpugin) ported C version to C++ using [entt](https://github.com/skypjack/entt) library.
- [@Heimdell](https://github.com/Heimdell) ported C version to OCaml.

# Results

![Gameplay](./screenshots/c_001.png)

## FPS

| Value           | Haskell | C     | Zig   | Rust  | D     | C++   |
|-----------------|---------|-------|-------|-------|-------|-------|
| Max FPS         | 500     | 25000 | 25078 | 24057 | 27778 | 24494 |
| Min FPS         | 3       | 3521  | 4031  | 2775  | 1376  | 3969  |
| Avg FPS         | 115     | 21482 | 21634 | 19809 | 22559 | 20875 |
| Avg to best (%) | 0.5     | 95.23 | 95.90 | 87.81 | 100   | 92.54 |
| Mean FPS        | 91      | 21739 | 21712 | 20695 | 22727 | 20777 |
| FPS dispersion  | 87      | 965   | 970   | 2445  | 2481  | 1439  |
| Dispersion %    | 75      | 4     | 4     | 12    | 11    | 7     |

FPS plots are built with [haskell utility](./haskell/plotting). Like:
```
cabal new-run plotting -- single fps.out
# or
cabak new-run plotting -- multiple Haskell ./fps_haskell.out Rust ./fps_rust.out Zig ./fps_zig.out
```
In haskell nix-shell.

![FPS](./fps_many.png)

### Haskell
FPS now stable 40-100, smooth gameplay, but there are noticeable freezes at, possibly, major GCs.
![Haskell FPS](./haskell/fps.png)

### C
There is smooth gameplay. Some drops can be explained by interference of other software in OS.
![C FPS](./c/asteroids/fps.png)

### Rust
Smooth gameplay. It was quite easy to implement the game and get high performance from first try.

![Rust FPS](./rust/fps.png)

I used specs as first implementation and than rewrote to legion for more cleaner code. Amount of code
and memory usage is dropped significantly. Also, I disabled legion multithreading as performance
with it drops to maximum 6000 FPS as the major time game sit in work stealer inside legion schedule system.
In single thread mode both ECS have near identical performance.

![ECS Compare](./rust/asteroids/ecs_compare.png)

### Zig
Very stable frame rate. Feels like great upgrade over C in development experience and performance.
![Zig FPS](./zig/asteroids/fps.png)

### D
There are two different compilers. First, DMD is original implementation of language. Second, LDC is
LLVM based implementation. DMD is usually slower than LDC, but in the particular case it's not. I have
no idea why DMD generates code that is FASTER for rendering dozens of asteroids than rendering empty screen.
LDC behaves like other LLVM based languages (stable constant FPS), but FPS slightly higher than the closest competitor (Zig).
I implemented custom ECS in the implementation and it feels like the closest to the "production" state.

Also I used LLVM PGO (Profiling Guided Optimizations) for LDC, that makes DMD and LDC results near identical.

![D FPS](./d/asteroids/fps_many.png)

### C++

Based on C implementation and [entt](https://github.com/skypjack/entt) library. Implementation provided by [@egorpugin](https://github.com/egorpugin).

![CPP FPS](./cpp/asteroids/fps.png)

### OCaml

Based on C implementation. Implementation provided by [@Heimdell](https://github.com/Heimdell). FPS degrades linearly from count of ever created entities, need to be optimized further. Also, sounds are not implemented.

![Ocaml FPS](./ocaml/fps_ocaml.png)

Comparing to C:
![Ocaml FPS](./ocaml/fps_compare.png)

## Line of Code

Libraries for ECS are not included in the LoC metric unless they are not embedded in the implementation:
```
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
C                               37            277            132           1529
Zig                             23            165            149           1097
Haskell                         25            157             37           1033
Rust                            23            113             74            842
D                               32            194            184            638
C++                              9             90             27            496
```

![Line of Codes](./code_lines.png)

## Memory

Measured with `command time -v <exe>`:

| Language | Max memory Kb | Max memory to best % |
|----------|---------------|----------------------|
| C        | 82760         | 187.29               |
| Zig      | 65984         | 149.33               |
| Haskell  | 83628         | 189.26               |
| Rust     | 45616         | 103.23               |
| D        | 68688         | 155.45               |
| C++      | 44188         | 100.00               |

![Memory table](./memory.png)

## My machine

It is important that all measurements of performance are done in equal environment. So, FPS benchmarks are executed on my personal PC that has:
* RTX 2080 Ti
* AMD Ryzen 9 3950X
* NixOS x86_64  
* SSD
* 64 GiB RAM
