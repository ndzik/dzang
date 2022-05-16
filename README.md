# Dzang

Dzang is a simply typed lambda calc language.
It currently utilizes a modified HM type inference algorithm and only allows very basic arithmetic.

# Goal

Dzang is an experiment for me to gain experience in writing a small library ([dzarser](https://github.com/ndzik/dzarser)), learning more about language design and implementations as well as applying functional patterns in a concrete project.

# Current State
Dzang can evaluate math expressions and type language constructs like lambda functions and their return types.

```
# Evaluation mode
$ cabal run dzang-exe
λf.λx.λy.(f x y) (λa.λb.a+b) 2 2
→ 4

# Typing mode
$ cabal run dzang-exe -- -t
λf.λx.λy.(f x y)
→ ∀a b c.{ ( ( a -> ( b -> c ) ) -> ( a -> ( b -> c ) ) ) }

λf.λx.λy.(f x y) (λa.λb.a+b)
→ { ( int -> ( int -> int ) ) }

λf.λx.λy.(f x y) (λa.λb.a+b) 2
→ { ( int -> int ) }

λf.λx.λy.(f x y) (λa.λb.a+b) 2 2
→ { int }
```

# Coming

In no specific order:

- [ ] LSP implementation
- [ ] Custom data types definition
- [ ] Dependent Types
- [ ] More language abstractions to actually make Dzang _useful_
