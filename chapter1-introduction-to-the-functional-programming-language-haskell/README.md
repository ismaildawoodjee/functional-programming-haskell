# Chapter 1: Introduction to the Functional Programming Language Haskell

In this chapter we give an introduction to the Haskell language. Here we will introduce the syntax of the language and informally explain the meaning of the language constructs. A formal definition of the semantics of the language follows in Chapter 2. For further descriptions of the Haskell language we refer to [Thi94, Bir98, Bir14, PJH98, Tho11, HPF00, Hud00, PJ00, Pep02]. More information about Haskell can be found on the Haskell homepage (<https://www.haskell.org>). The Haskell compiler and interpreter GHC is also available here within the *Haskell Platform*.

We first introduce the basic language constructs of Haskell in Section 1.1. We then go on to discuss functional programming techniques. To this end, in Section 1.2 we consider higher-order functions, i.e., functions that in turn process functions. In Section 1.3, we show how to program with lazy evaluation, using infinite data objects. Finally, in Section 1.4 we discuss the concept of monads, which are used in particular for input and output in Haskell.

## 1.1 Basic Language Constructs

In this section, we introduced the basic language constructs of Haskell (declarations, expressions, patterns, and types).

### 1.1.1 Declarations

A program in Haskell is a sequence of declarations. The declarations must be left-aligned. The reason for this will become clear later when we consider local declarations that are indented (or on the same line).

A declaration is (in the simplest case) a description of a function. Functions are characterized by a function symbol (the name of the function), the definition range, the value range of the result and a mapping rule. The definition range and the value range are specified in a so-called type declaration and the mapping rule is described in a function declaration. The syntax of declarations is therefore given by the following context-free grammar. [A declaration can be either a type declaration or a function declaration]

$\underline{\text{decl}} \rightarrow \underline{\text{typedecl}} \text{ | } \underline{\text{fundecl}}$

In the following, we will always indicate non-terminal symbols by underlining them. Furthermore, we will start with a subset of Haskell and extend the grammar rules successively. (We will not consider some programs allowed in the Haskell syntax - the complete grammar for Haskell programs can be found in [PJH98]).

Text enclosed between `{-` and `-}` and any text between `--` and the end of the line are considered comments in Haskell.

#### Type Declarations

Variable identifiers serve as function symbols in Haskell. For example, for a function that squares numbers, the name `square` can be used. A declaration binds a variable (like `square`) to a value (like the function that squares numbers). Then, one can specify the following type declaration for `square`.

```haskell
square :: Int -> Int
```

The first `Int` describes the definition range (domain) and the second `Int` describes the value range (range) of `square`. The type `Int` is predefined in Haskell. The declaration `var :: type` means that the variable `var` has the type `type`. With the help of "`->`" a function type is defined (i.e., `Int -> Int` is the type of functions that map integers into integers). As another example, `[Int]` describes the type of lists of integers. In general, for each type `a` there exists the type `[a]` of lists with elements of type `a`.

You get the following grammar rule for the syntax of type declarations. A type declaration specifies the type of one or more variables.

$\underline{\text{typedecl}} \rightarrow \underline{\text{var}}_1,\ldots,\underline{\text{var}}_n \text{ :: } \underline{\text{type}} \text{, for all } n \geq 1$

Type declarations do not have to be specified. They are then calculated automatically by the interpreter or compiler. However, type declarations are beneficial for the understandability of programs and should therefore normally be used. These declarations are then checked by the interpreter or compiler.

Variable identifiers `var` are arbitrary sequences of letters and numbers (strings) that start with a lowercase letter (such as `square`).

#### Function Declarations

The type declaration is followed by the defining equations, i.e. the mapping rule. For example, the function declaration for `square` could be as follows.

```haskell
square x = x * x
```
