Simple simulation of circles colliding and splitting in Haskell and Rust.
Intentionally CPU heavy (e.g., doesn't use VBOs for rendering, doesn't calculate transforms in shaders).
Straightforward single-threaded code with very little manual optimization.

# Requirements:
- Install libsdl2-dev

# Conclusions:

- Haskell is more concise and easier to read and write
- Memory usage is similar, around 18MB
- FPS is similarly steady around 59.5-60.0, with occasional drops to 57/58 in both. No obvious GC pauses for Haskell. 
- Performance is similar:

| Triangles per circle | Haskell (circles @ 55fps)| Rust (circles @ 55fps) | Haskell/Rust |
|---|---|---|---|
| 16 | 1352 | 2843 | 0.48 |
| 32 | 1310 | 2273 | 0.58 |
| 64 | 1174 | 1787 | 0.66 |
| 128 | 965 | 1190 | 0.81 |
| 256 | 689 | 705 | 0.98 |
| 512 | 383 | 379 | 1.01 |
| 1024 | 217 | 205 | 1.06 |
| 2048 | 110 | 99 | 1.11 |
| 4096 | 53 | 40 | 1.32 |

Measured with:
- CPU: Intel i5-7200U @ 2.50GHz × 4
- Memory: 16GB
- Graphics: Mesa Intel HD Graphics 620 (KBL GT2))

# Notes

## Haskell:
- Enabling optimization is in cabal.project.local
- Annotating strict data fields and compiling with `ghc-options: -fllvm -funbox-strict-fields -fexcess-precision` makes a huge difference.
- Using vectors everywhere had much worse performance than lists where they are used only for accumulation and single iteration
- The functions `quot` and `rem` are the equivalents of Rust's `/` and `%` (as opposed to `div` and `mod`). They also perform much better. 
- Manually unboxed arithmetic didn't improve performance
- To profile, uncomment profiling in cabal.project.local and then run with:
```
$ cabal install profiteur
$ cabal run haskell-circles -- +RTS -pa -sstderr && profiteur haskell-circles.prof && firefox haskell-circles.prof.html
```

## Rust:
- Run with `cargo run --release`
- Allocating vector with capacity and pushing into it was not faster than mapping and collecting