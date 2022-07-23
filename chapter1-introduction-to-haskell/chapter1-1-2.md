# Chapter 1: Introduction to the Functional Programming Language Haskell

## 1.1 Basic Language Constructs

In this section, we introduce the basic language constructs of Haskell (declarations, expressions, patterns, and types).

### 1.1.2 Expressions

Expressions $\underline{\text{exp}}$ are the central concept of functional programming. An expression describes a value (e.g. a number, a letter or a function). The input of an expression into the interpreter causes its evaluation. In addition, each expression has a type. When "`:t` $\underline{\text{exp}}$"is entered in the (interactive mode of) GHC, the type of $\underline{\text{exp}}$ is calculated and output. When evaluating an expression, it is also first checked whether the expression is correctly typed and only in case of success the evaluation is actually performed. An expression $\underline{\text{exp}}$ can have the following form:
