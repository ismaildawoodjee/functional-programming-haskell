# Chapter 1: Introduction to the Functional Programming Language Haskell

## 1.1 Basic Language Constructs

In this section, we introduce the basic language constructs of Haskell (declarations, expressions, patterns, and types).

### 1.1.2 Expressions

Expressions $\underline{\text{exp}}$ are the central concept of functional programming. An expression describes a value (e.g. a number, a letter or a function). The input of an expression into the interpreter causes its evaluation. In addition, each expression has a type. When "`:t` $\underline{\text{exp}}$"is entered in the (interactive mode of) GHC, the type of $\underline{\text{exp}}$ is calculated and output. When evaluating an expression, it is also first checked whether the expression is correctly typed and only in case of success the evaluation is actually performed. An expression $\underline{\text{exp}}$ can have the following form:

* $\underline{\text{var}}$: Variable identifiers like `x` are expressions. As mentioned, variable identifiers in Haskell are formed by strings starting with a lowercase letter.

* $\underline{\text{constr}}$: Another possibility for expressions are data constructors. Data constructors are used to construct objects of a data structure and are introduced during the data type definition. In Haskell, identifiers for data constructors are formed by strings that begin with uppercase letters. Examples are the data constructors `True` and `False` of the predefined data structure `Bool`. Another example are the data constructors `[]` and `:` for the predefined data structure of lists.

* $\underline{\text{integer}}$: Also the integers `0, 1, -1, 2, -2, ...` are expressions.

* $\underline{\text{float}}$: Floating point numbers like `-2.5` or `3.4e+23` are also expressions.

* $\underline{\text{char}}$: Other expressions are `a,...,z,A,...,Z,0,...,9` as well as the space character `' '` and non-printable control characters like `\n` for the end-of-line character. All these characters are evaluated to themselves (in apostrophes (quotes)).

* $[\underline{\text{exp}}_1, \ldots, \underline{\text{exp}}_n], \text{ where } n \geq 0$: Such an expression denotes a list of `n` expressions. As mentioned, `[]` represents the empty list and `[0,1,2,3]` is an abbreviation for `0 : 1 : 2 : 3 : []`, where `:` associates to the right. All elements of a list must have the same type. For example, the type of the above list would be `[Int]`, i.e. the type of lists of integers.

* $\underline{\text{string}}$: A $\underline{\text{string}}$ is a list of characters $\underline{\text{char}}$ (i.e., it is an expression of type `[Char]`). Instead of `['h','e','l','l','o']` one often writes `"hello"`. Such a string is evaluated to itself. The predefined type `String` in Haskell is identical with the type `[Char]`.

* $(\underline{\text{exp}}_1, \ldots, \underline{\text{exp}}_n), \text{ where } n \geq 0$: This is a tuple of expressions. Unlike the list, the expressions in a tuple can be of different types. An example would be the expression `(10, False)`. This expression would have the type `(Int, Bool)`. One-element tuples ($\underline{\text{exp}}$) are evaluated to $\underline{\text{exp}}$. Zero-element tuples `()` have the special type `()`.

* $(\underline{\text{exp}}_1, \ldots, \underline{\text{exp}}_n), \text{ where } n \geq 2$: Such an expression stands for the function application of expressions. Here we omit the parenthesis as far as possible. The function application has the highest binding priority and associates to the left. Examples for such expressions are `square 10` (of type `Int`) or `plus 5 3` (also of type `Int`) or `plus 5` (of type `Int -> Int`). So the value of an expression can be a function again.

* $\texttt{if} \text{ } \underline{\text{exp}}_1 \text{ } \texttt{then} \text{ } \underline{\text{exp}}_2 \text{ } \texttt{else} \text{ } \underline{\text{exp}}_3$: Here $\underline{\text{exp}}_1$ must be of type `Bool` and $\underline{\text{exp}}_2$ and $\underline{\text{exp}}_3$ must be of the same type. During the evaluation, the value of $\underline{\text{exp}}_1$ is determined first and then, depending on this value, the value of $\underline{\text{exp}}_2$ or $\underline{\text{exp}}_3$. Instead of

```haskell
maxi(x, y) | x >= y    = x
           | otherwise = y
```

one can therefore also write the following:

```haskell
maxi(x, y) = if x >= y then x else y
```
