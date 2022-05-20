# Dzang

Dzang is a simply typed lambda calc language.
It currently utilizes a modified HM type inference algorithm and only allows very basic arithmetic.

# Goal

Dzang is an experiment for me to gain experience in writing a small library ([dzarser](https://github.com/ndzik/dzarser)), learning more about language design and implementations as well as applying functional patterns in a concrete project.

# Current State
Dzang can evaluate math expressions and type language constructs like lambda functions and their return types.

```
cabal run dzang-exe
a=1
▷ a: int
b=2
▷ b: int
a+b
▷ 3: int
f=λg.λx.λy.(g x y)
▷ f: ∀a b c.( a → b → c ) → a → b → c
add=λx.λy.(x+y)
▷ add: int → int → int
add 1 2
▷ 3: int
add a b
▷ 3: int
```

# Coming

In no specific order:

- [ ] LSP implementation
- [ ] Custom data types definition
- [ ] Dependent Types
- [ ] More language abstractions to actually make Dzang _useful_
