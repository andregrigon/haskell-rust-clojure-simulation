Simple simulation of squares colliding and splitting in Haskell and Rust.

Requirements:
- Install sdl2

Conclusion:
- Haskell is more concise and easier to read and write
- Rust is around 2x faster:
  - Haskell maintains 60fps with 1500 squares
  - Rust maintains 60fps with 3000 squares

Haskell:
- Enable profiling in ~/.cabal/config and then profile with:
```
$ cabal install profiteur
$ cabal run haskell-squares -- +RTS -pa -sstderr && profiteur haskell-squares.prof && firefox haskell-squares.prof.html
```
- Using vectors everywhere had much worse performance than lists where they are used only for accumulation
- Manually unboxed arithmetic didn't improve performance
- Strict data helps

Rust:
- Run with `cargo run --release`