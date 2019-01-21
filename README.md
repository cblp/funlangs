<!-- DO NOT EDIT THIS FILE -->
<!-- edit funlangs.hs instead -->

# Functional languages

There is no such thing as a functional language.

[You can](https://lispcast.com/fp-in-my-language) write functionally
in any language.

Also, different languages have different functional features.

## Functional features

| Feature | C | C++ | Haskell | Idris | Java | OCaml | Python | Rust | Scala |
|---|---|---|---|---|---|---|---|---|---|
| Closures | :x: | :warning: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :heavy_check_mark: | :warning: | :heavy_check_mark: |
| Downwards Funarg Problem | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Functions | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Guaranteed Tail Call Optimization |  |  | :heavy_check_mark: | :heavy_check_mark: | :x: | :heavy_check_mark: |  |  | :warning: |
| Lambda Abstraction Syntax | :x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :warning: | :heavy_check_mark: | :heavy_check_mark: |
| Pure Functions | :x: | :warning: | :heavy_check_mark: | :heavy_check_mark: | :x: | :x: | :x: | :x: | :x: |
| Upwards Funarg Problem | :x: | :warning: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |

## Supporting features

| Feature | C | C++ | Haskell | Idris | Java | OCaml | Python | Rust | Scala |
|---|---|---|---|---|---|---|---|---|---|
| Ad Hoc Polymorphism | :x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Algebraic Data Types | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :x: | :heavy_check_mark: | :heavy_check_mark: |
| Dependent Types | :x: | :x: | :x: | :heavy_check_mark: |  | :x: | :x: | :x: | :x: |
| Forces Immutability | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :x: | :heavy_check_mark: | :warning: |
| Immutable Data | :x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Laziness By Default | :x: | :x: | :heavy_check_mark: | :x: |  | :x: | :x: | :x: | :x: |
| Parametric Modules | :x: | :x: | :warning: | :heavy_check_mark: |  | :heavy_check_mark: | :x: | :x: | :heavy_check_mark: |
| Parametric Polymorphism |  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Pattern Matching |  | :warning: | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: | :warning: | :warning: | :heavy_check_mark: |
| Pattern Matching Alternatives |  |  | :heavy_check_mark: | :heavy_check_mark: |  | :heavy_check_mark: |  | :heavy_check_mark: | :heavy_check_mark: |
| Pattern Matching Variable Introduction |  |  | :heavy_check_mark: | :heavy_check_mark: |  |  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Polymorphic Recursion | :x: | :x: | :heavy_check_mark: | :heavy_check_mark: | :x: | :heavy_check_mark: | :warning: |  | :heavy_check_mark: |
| Referential Transparency |  |  | :heavy_check_mark: | :heavy_check_mark: |  |  |  |  | :x: |
| Row Polymorphism | :x: | :x: | :warning: |  | :x: | :heavy_check_mark: | :x: |  |  |
| Static Typing | :warning: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :x: | :heavy_check_mark: | :heavy_check_mark: |
| Totality Checking | :x: | :x: | :x: | :heavy_check_mark: |  | :x: | :x: | :x: | :x: |
| Uniqueness Types | :x: | :x: | :x: | :heavy_check_mark: |  | :x: | :x: | :heavy_check_mark: | :x: |
| Universe Polymorphism | :x: | :x: | :x: | :heavy_check_mark: |  | :x: | :x: | :x: | :x: |

## Scores

A well implemented feature counts as 1,
a hard-to-use one counts as 0.5.
Supporting featurues count twice less.

| Language | Score |
|----------|-------|
| Idris | 15.0 |
| Haskell | 13.5 |
| OCaml | 11.5 |
| Scala | 10.75 |
| Rust | 9.25 |
| Python | 7.0 |
| C++ | 6.75 |
| Java | 5.0 |
| C | 2.25 |

