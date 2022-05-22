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
g = λf.λx.λy.(f x y)
▷ g: ∀a b c.( a → b → c ) → a → b → c
add = λa.λb.(a+b)
▷ add: int → int → int
h = g add
▷ h: int → int → int
add 4 5
▷ 9: int
h 5 4
▷ 9: int
```

# Coming

In no specific order:

- [ ] LSP implementation
- [ ] Custom data types definition
- [ ] Dependent Types
- [ ] More language abstractions to actually make Dzang _useful_
