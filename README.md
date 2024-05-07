# Dependent Types

This repository contains a compiler for a dependently typed language.
The language borrows concepts and syntax from Idris, and is implemented in Haskell.

## Usage

The project depends on the following tools:

- [GHC](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [Racket](https://racket-lang.org/)

These can be installed through the above links or using a package manager.
On Ubuntu, the command `sudo apt install ghc cabal-install racket` will install the necessary tools.

The project can be built with `cabal build`, and the compiler can be run with `cabal run dependent-types -- <file>`.
Examples to run can be found in the `examples/` directory.
This produces a Racket file that can be run with `racket <file>.rkt`.

The test suite can be run with `cabal test --test-show-details=always --test-option "--color=always"`.

## Roadmap

- Parser
  - [x] AST powered by [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
  - [x] Expressions
  - [x] Top-level definitions
- Static analysis
  - [x] Dependent typing
    - [x] Evaluation
    - [x] Normalization
    - [x] Unification
    - [x] Bidirectional checking
- Compiler
  - [x] Scheme backend
- Language support
  - [x] Prelude of useful types
  - [ ] Examples
  - [ ] Documentation
- Tooling
  - [x] CLI
  - [x] Pretty errors powered by [diagnose](https://hackage.haskell.org/package/diagnose)
