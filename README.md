# microML

**for UCL MSc Computer Science.**


microML is a simple functional programming language designed for the *BBC micro:bit* microcomputer. It is
essentially a lisp, with a bit of ml style sugar on the syntax.

This is still under active development so please don't try to use it yet! However, if you really really want,
there is a repl you can use for a quick feel of the language.

*Work in progress*

Here's a quick overview of the syntax:

Declarations (simple and recursive) are introduced with a _let_

```ml
microML ⊦ let x = 5
microML ⊦ 5 : Number
microML ⊦ let incBy1 x = x + 1
microML ⊦ :type inc
microML ⊦ inc : Number -> Number
```

or 

```ml
microML ⊦ let incBy1 = \x -> x + 1
microML ⊦ incBy1 2
microML ⊦ 3 : Number
```

More complex things can be entered at the repl as well (at the moment everything must be on one line)

```ml
microML ⊦ let compose x y = \z -> x (y z)
microML ⊦ compose inc inc 2
microML ⊦ 4 : Number

microML ⊦ let fact n = if n == 0 then 1 else n * (fact (n-1))
microML ⊦ fact 5
microML ⊦ 120 : Number
```

TODO
====

My first attempt at writing anything more than an individual function in Haskell... and it shows it.
Lots of good stuff here, but also a lot that needs to be rewritten when there's the time now that I
understand a bit more about how to do things.

+ Working on type inference at the moment...
+ Compiler to C++ (for the micro:bit)
