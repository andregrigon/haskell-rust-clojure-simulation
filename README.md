Simple simulation of circles colliding and splitting in Haskell, Rust and Clojure.
Intentionally CPU heavy (e.g., doesn't use VBOs for rendering, doesn't calculate transforms in shaders).
Straightforward single-threaded code with little manual optimization.

![haskell-circles](https://user-images.githubusercontent.com/2816910/167230510-1e8f3246-563d-4362-9b54-148d5a812d8b.gif)

# Requirements:
- For Haskell and Rust install libsdl2-dev

# Conclusions:

- Code:
  - Haskell: original code and had most previous experience with; had most optimization work done
  - Rust: very easy port from Haskell, worked first time; had least optimization work
  - Clojure: easy port but many ClassCastException/NullPointerExceptions before running successfully, some very annoying to understand
- Memory:
  - Haskell and Rust: around 18MB
  - Clojure: around 400MB
- FPS drops:
  - Haskell and Rust: similarly steady around 59.5-60.0, with occasional drops to 57/58 in both.
  - Clojure: steady around 59.0-60.0; didn't see further drops before 55fps cutoff, but it came much earlier than Haskell and Rust (see table)
  - No obvious GC pauses for Haskell or Clojure.
- Overall FPS per circle count:

| Triangles per circle | Haskell (circles @ 55fps) | Rust (circles @ 55fps) | Clojure (circles @ 55fps) | Haskell/Rust | Haskell/Clojure |
|---|---|---|---|---|---|
| 16 | 1352 | 2843 | 192 | 0.48 | 7.04 |
| 32 | 1310 | 2273 | 201 | 0.58 | 6.52 |
| 64 | 1174 | 1787 | 168 | 0.66 | 6.98 |
| 128 | 965 | 1190 | 135 | 0.81 | 7.15 |
| 256 | 689 | 705 | 97 | 0.98 | 7.10 |
| 512 | 383 | 379 | 51 | 1.01 | 7.50 |
| 1024 | 217 | 205 | 27 | 1.06 | 8.03 |
| 2048 | 110 | 99 | 16 | 1.11 | 6.88 |
| 4096 | 53 | 40 | 0 | 1.32 | - |

Measured with:
- CPU: Intel i5-7200U @ 2.50GHz × 4
- Memory: 16GB
- Graphics: Mesa Intel HD Graphics 620 (KBL GT2)
- Average of 3 runs

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

## Clojure:
- Adding `(set! *unchecked-math* :warn-on-boxed)` and fixing all warnings didn't seem to change performance (and was very laborious)
- Explicitly avoiding transducer chunking had a 5-10% performance increase when testing with hundreds of circles
