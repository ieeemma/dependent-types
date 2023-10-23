# Dependent Types

A yet unnamed dependently typed language and compiler.

## Roadmap

Features marked with `*` are not required for the core language but may be implemented if time allows.

- Parser
  - [x] AST powered by [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
  - [x] Expressions
  - [ ] Toplevels
  - [ ] Whitespace sensitive parsing similar to Haskell
  - [ ] Custom infix operators
  - [ ] \* Anyfix operator parsing similar to Agda
- Static analysis
  - [ ] Dependent typing
    - [ ] Evaluation
    - [ ] Normalization
    - [ ] \* Unification for holes
    - [ ] \* Implicit arguments
  - [ ] Name resolution
  - [ ] \* Type classes
  - [ ] \* Totality checking similar to Idris
- Compiler
  - [ ] LLVM backend
  - [ ] Runtime + garbage collection
  - [ ] \* SSA optimization
- Language support
  - [ ] Prelude of useful types
  - [ ] Documentation for prelude
  - [ ] \* More advanced libraries eg. proof helpers
  - [ ] \* CFFI
- Tooling
  - [ ] CLI
  - [ ] Pretty errors powered by [diagnose](https://hackage.haskell.org/package/diagnose)
  - [ ] \* Formatter powered by [prettyprinter](https://hackage.haskell.org/package/prettyprinter)

## Usage

A file can be compiled using `cabal run dependent-types -- file`.
Test suite can be run using `cabal run test:tests`.
