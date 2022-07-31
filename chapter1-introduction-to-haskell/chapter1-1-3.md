# Chapter 1: Introduction to the Functional Programming Language Haskell

## 1.1 Basic Language Constructs

In this section, we introduce the basic language constructs of Haskell (declarations, expressions, patterns, and types).

### 1.1.3 Patterns

In the function declaration, the so-called "patterns" are specified for the arguments. They restrict the form of the allowed arguments. They restrict the form of the allowed arguments. The syntax of patterns is therefore similar to the syntax of expressions, because patterns are prototypes for the expected values. The form of the values is described by the occurring data constructors, where instead of some partial values there are variables in the pattern (so now we use data constructors to decompose objects instead of constructing them). A pattern matches an expression (or it matches this expression) if this expression emerges from the pattern when the variables are replaced by other partial expressions. As an example we had already considered the algorithms and, len and second in Section $1.1.1$.

As another example, consider the algorithm `append`. (An analogous (infix)- function `++` (on lists with elements of arbitrary type) is predefined in Haskell).

```haskell
append :: [Int]  -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : append xs ys
```

To compute `len (append [1] [2])`, the argument `append [1] [2]` of `len` is evaluated only until one can decide which pattern in the definition of `len` matches. So here one evaluated the argument only to `1:append [] [2]`. At this point it is already clear that only the second equation of len is usable and one gets `1 + len (append [] [2])`, which is then further evaluated. If it is not possible to determine whether the pattern under consideration matches without evaluating the argument, the argument expression is first evaluated only until the outermost constructor of the argument is determined. (This is called Weak Head Normal Form, cf./compare with Chapter 3.) Then one can check whether this constructor matches the outermost constructor of the pattern. If necessary, another recursive call of the pattern matching procedure for the partial arguments can be made.

For example, consider the following definitions.

```haskell
zeros :: [Int]
zeros = 0 : zeros

f :: [Int] -> [Int] -> [Int]
f [] ys = []
f xs [] = []
```

The evaluation of `f [] zeros` terminates, although `zeros` by itself does not terminate. The reason is that no evaluation of `zeros` is necessary to find out that the first equation of `f` is applicable. But also `f zeros []` terminates. Here `zeros` is first evaluated in one step to `0 : zeros`. Now the outermost constructor `:` of `f`'s first argument is fixed. Since this constructor is different from the constructor `[]`, the first equation cannot be applicable and therefore the second equation is used.

An example of the use of pattern matching in pattern declarations is

```haskell
let x:xs = [1,2,3] in xs
```
