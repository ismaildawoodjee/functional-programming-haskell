# Chapter 1: Introduction to the Functional Programming Language Haskell

## 1.1 Basic Language Constructs

In this section, we introduce the basic language constructs of Haskell (declarations, expressions, patterns, and types).

### 1.1.2 Expressions

Expressions $\underline{\text{exp}}$ are the central concept of functional programming. An expression describes a value (e.g. a number, a letter or a function). The input of an expression into the interpreter causes its evaluation. In addition, each expression has a type. When "`:t` $\underline{\text{exp}}$"is entered in the (interactive mode of) GHC, the type of $\underline{\text{exp}}$ is calculated and output. When evaluating an expression, it is also first checked whether the expression is correctly typed and only in case of success the evaluation is actually performed. An expression $\underline{\text{exp}}$ can have the following form:

* $\underline{\text{var}}$: Variable identifiers like `x` are expressions. As mentioned, variable identifiers in Haskell are formed by strings starting with a lowercase letter.

* $\underline{\text{constr}}$: Another possibility for expressions are data constructors. Data constructors are used to construct objects of a data structure and are introduced during the data type definition. In Haskell, identifiers for data constructors are formed by strings that begin with uppercase letters. Examples are the data constructors `True` and `False` of the predefined data structure `Bool`. Another example are the data constructors `[]` and `:` for the predefined data structure of lists.
