# Chapter 0: Introduction

For computer scientists, knowledge of different families of programming languages is necessary for several reasons:

- Familiarity with different concepts of programming languages allows us to better express own ideas in the development of software.
- The background knowledge of different programming languages is necessary to choose the most suitable language in concrete projects.
- If one has already learned some conceptually different programming languages, it is very easy to quickly learn other programming languages later on.
- It is also the task of computer scientists to design new programming languages. This can only be done on the basis of the languages already developed.

Generally, we distinguish between imperative and declarative programming languages (declarative languages are further subdivided into functional and logical languages). In imperative languages, programs are composed of a sequence of statements which are statements that change the values of variables in memory. Most of the programming languages in use today are based on this principle, which also has a direct connection to the classical computer architecture which goes back to John von Neumann.

In declarative programming, on the other hand, programs consist of a specification of what is to be computed. The specification, how the computation is to run exactly, is left to the interpreter and/or the compiler. Declarative programming languages are therefore problem-oriented instead of machine-oriented.

On the one hand, the different programming languages are all "uniform", i.e., any program can in principle be written in any of the commonly used languages. On the other hand, however, the languages are differently suited for different application areas. For example, imperative languages such as C are used for fast machine-oriented programming, since there the programmer can (and must) directly take over the management of the memory. In other programming languages, this task is performed automatically (by the compiler). This allows a faster program development which is less error-prone. On the other hand, the resulting programs are usually less efficient (i.e., they require more time and memory). Functional languages are mainly used for prototyping, in telecommunications, but also for multimedia applications.

The following example is intended to illustrate the difference between imperative and functional programming languages. We use two typical languages, Java and Haskell. To illustrate these programming paradigms, we will consider the algorithm for computing the length of a list. The input of the algorithm is a list like `[15, 70, 36]` and the output of the algorithm is the length of this list (in our example `3`). An imperative algorithm for solving this task can easily be given.

```java
class Element {
  Data value;
  Element next;
}

class List {
  Element head;

  static int len (List l) {
    int n = 0;

    while (l.head != null) {
      l.head = l.head.next;
      n = n + 1;
    }
    return n;
  }
}
```

Using this program, one can make the following observations:

- The program consists of individual instructions that are processed one after the other. For this, there are various control structures (conditions, loops, etc.) to control the program flow.

- The execution of an instruction changes the values of the variables in memory. Each instruction can therefore trigger side effects. For example, during the execution of `len`, the value of both `n` and `l` changes. The latter also has effects outside the len method. When `len(l)` is computed, the value of the object pointed to by `l` is also changed. So not only do you get an `int` value as a result, but as a side effect, `l` is changed in the process (in the end, `l.head = null`), i.e., the list is emptied when calculating the length.

- The programmer must think about the realization and memory management of non-primitive data types (like lists). For example, the above side effect may not be desired. To avoid this, however, the programmer must anticipate desired and undesired side effects. The side effects and the explicit memory management in imperative programs therefore often lead to errors which are difficult to localize.

Having illustrated the concepts of imperative programming, we now come to declarative programming languages, where the program describes what is to be computed, but leaves the exact specification of how the computation is to proceed to the compiler or interpreter. Our goal is again to calculate the length of a list. The following description makes clear what the length `len` of a list `l` means:

- $\textbf{(A)}$ If the list `l` is empty, then `len(l) = 0`.

- $\textbf{(B)}$ If the list `l` is not empty and "`xs`" is the list `l` without its first element, then `len(l) = 1 + len(xs)`.
  
In the following we write `x:xs` for the list that is created from the list `xs` by inserting the element (or value) `x` in front. So we have `15:[70,36] = [15,70,36]`.
Every non-empty list can therefore be represented as `x:xs`, where `x` is the first element of the list and `xs` is the remaining list without its first element. If we denote the empty list with `[]`, we have `15:70:36:[] = [15,70,36]` (where the insertion with "`:`" is processed from right to left, i.e., "`:`" associates to the right).

Now the above specification (description) of the function `len` can be directly translated into a functional program. The implementation in the functional programming language Haskell is as follows:

```haskell
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs
```

A functional program is a sequence of declarations. A declaration binds
a variable (like `len`) to a value (like the function that calculates the length of lists). The line `len :: [a] -> Int` is a type declaration that says that `len` expects a list as input and calculates an integer as output. Here `a` denotes the
type of the elements of the list. The data structure of the lists is predefined in Haskell (but of course there is the possibility to define further data structures by yourself).

The defining equations of `len` follow. The first equation specifies what the
result of the function `len` is, if `len` is applied to the empty list `[]`. (In Haskell
parentheses around the argument of a function are not necessary, but they can also be written. So, for the first defining equation you could also use `len([]) = 0`
and for the second equation `len(x:xs) = 1 + len(xs))`. The second equation
is applicable if the argument of `len` is a non-empty list. The argument has
then the form `x:xs`. In this case, `1 + len xs` is now calculated. It can be seen that
`len` is defined recursively, i.e., to compute `len(x:xs)`, `len` must be computed again (from another argument, namely `xs`).

The execution of a functional program consists of the evaluation of an expression with the help of these functions. In our example, only the function `len` is defined. To illustrate the operation of the above algorithm, we consider the computation of `len [15,70,36]`. When the algorithm is executed, it first checks whether the argument is the empty list (i.e., whether the argument `[]` in the first defining equation matches the current argument `[15,70,36]`). This process is called pattern matching. Since this is not the case, one now tries to apply the second defining equation. This is possible, where in our example the first list element `x` corresponds to the number `15` and the remaining list `xs` corresponds to the list `[70,36]`. Thus, to compute `len [15,70,36]`, one must determine `1 + len [70,36]`. Since the new argument `[70,36]` is again a non-empty list with the `x` value `70` and the `xs` value `[36]`, this leads to a new call of `len` with the argument `[36]`. Finally, the result is `1 + 1 + 1 + 0 = 3`.

The most important features of the functional programming language Haskell can be can be summarized as follows:

- $\textbf{No Loops}$
  
  In (purely) functional languages there are no control structures like loops, instead only recursion is used.

- $\textbf{ Polymorphic Type System}$

  Functional programming languages usually allow a function like `len` to be used for lists of elements of arbitrary types. This is called parametric polymorphism. Thus, for the variable `a` in the type of `len`, any type of elements can be used (`a` is a type variable), i.e., `len` works independently of the type of the elements. Nevertheless, the type system guarantees that data will not be misinterpreted.

  Parametric polymorphism results in a reduction of the programming effort, since corresponding functions do not have to be rewritten again and again.

- $\textbf{No Side Effects}$

  The order of calculations does not affect the result of the program. In particular, programs do not have side effects, i.e., the value of the parameters does not change when a function is executed. Thus, the result of a function depends only on the arguments of the function, and if a function is applied several times to the same arguments, the result is always the same. This behavior is called referential transparency.

- $\textbf{ Automatic Memory Management}$

  Explicit pointer manipulation and requesting or freeing memory are not performed by the programmer.

- $\textbf{Equal Treatment of Functions as Data Objects}$
  
  Functions are treated as equal data objects. In particular, functions can also be arguments or results of other functions. Such functions are called higher order functions.

- $\textbf{Lazy Evaluation}$

  To evaluate a function call, only those parts of the arguments are evaluated that are necessary for calculating the result. (However, this evaluation strategy is not used in all functional languages. For example, languages like ML or Lisp and Scheme work with so-called strict evaluation strategy, where all arguments of a function must be evaluated before the function itself can be applied).

Overall, the following important advantages of functional programming arise:

- The programs are shorter, clearer and easier to maintain.

- Fewer errors occur during programming, resulting in more reliable programs. Functional languages usually also have a clear mathematical basis and are better suited for verification than imperative programming languages.

- Program development is faster and easier (This is also due to the fact that the programmer has to deal much less with memory organization than in imperative languages). For example, the company Ericsson has measured a factor of 10-25 faster program development time after the introduction of the functional language Erlang. The resulting programs were shorter by a factor of 2-10 than before. In particular, this shows that functional languages are ideal for prototype development. For very time-critical (real-time) applications, machine-oriented languages are often more suitable for efficiency reasons.

- Functional programs are often more reusable and have a more modular structure.

The structure of the lecture is as follows: In Chapter 1, an introduction to the functional programming language Haskell is given. Through this, the possibilities of functional languages become clear and one gains an impression of a real functional programming language.

In the following chapters, we review the concepts and techniques that underlie functional programming languages, again referring to Haskell. In Chapter 2 we show how to define the semantics of such languages. This is a formal definition of what a functional program or an expression in a functional program means (i.e., what result is computed by it). Such a definition is necessary to determine the correctness of programs and to define what the constructs of the programming language mean. In particular, it is therefore the basis for any implementation of the language, since only through it can be determined how interpreters or compilers should work.

We then introduce the so-called Lambda Calculus in chapter 3. This is the basic language that underlies all functional programming languages. These programming languages are merely more readable versions of the lambda calculus. We therefore show how Haskell can be traced back to lambda calculus. In particular, the lambda calculus provides a way to implement functional programs and check them for certain correctness properties.

To this end, in Chapter 4 we present a procedure that examines whether a program (or the corresponding expression in the lambda calculus) is correctly typed. In other words, we check whether functions are always applied only to arguments of the right kind. This check is performed in interpreters or compilers as the first step before executing a functional program. As mentioned, Haskell has a polymorphic type system. This allows, for example, to realize the computation of the length of lists of numbers, of lists of characters, of lists of lists, etc. with one and the same function `len`, which allows a high degree of reusability. On the other hand, type calling becomes non-trivial for this reason.

I would like to thank Rene Thiemann, Peter Schneider-Kamp, Carsten Fuhs, Darius Dlugosz, and Diego Biurrun for their constructive comments and suggestions while proofreading the script.
