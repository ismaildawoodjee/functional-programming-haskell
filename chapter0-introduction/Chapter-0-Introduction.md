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
