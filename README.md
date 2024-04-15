# Dependent Types

A yet unnamed dependently typed language and compiler.

## Roadmap

Features marked with `*` are not required for the core language but may be implemented if time allows.

- Parser
  - [x] AST powered by [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
  - [x] Expressions
  - [x] Toplevels
  - [ ] \* Custom infix operators
  - [ ] \* Anyfix operator parsing similar to Agda
- Static analysis
  - [x] Dependent typing
    - [x] Evaluation
    - [x] Normalization
    - [ ] \* Unification for holes
    - [ ] \* Implicit arguments
  - [ ] \* Type classes
  - [ ] \* Totality checking similar to Idris
- Compiler
  - [x] Scheme backend
  - [ ] \* Optimization
- Language support
  - [ ] Prelude of useful types
  - [ ] Documentation for prelude
  - [ ] \* More advanced libraries eg. proof helpers
  - [ ] \* CFFI
- Tooling
  - [ ] CLI
  - [x] Pretty errors powered by [diagnose](https://hackage.haskell.org/package/diagnose)

## Usage

A file can be compiled using `cabal run dependent-types -- file`.
Test suite can be run using `cabal run test:tests`.
