Simple simulation of squares colliding and splitting in Haskell and Rust.

Requirements:
- Install sdl2

Conclusion:
- Haskell is more concise and easier to read and write
- Rust is 2x faster:
  - Haskell maintains 60fps with 1600 squares
  - Rust maintains 60fps with 3000 squares

Haskell:
- Enable profiling in ~/.cabal/config and then profile with:
```
$ cabal install profiteur
$ cabal run haskell-squares -- +RTS -pa -sstderr && profiteur haskell-squares.prof && firefox haskell-squares.prof.html
```
- Using vectors everywhere had worse performance than using lists when they were small
- Manually unboxed arithmetic didn't improve performance


Rust:
- Run with `cargo run --release`