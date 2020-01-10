[![Build Status](https://travis-ci.com/rwetherall/memofunc.svg?token=x2QLvsytRz6d82hRES7c&branch=master)](https://travis-ci.com/rwetherall/memofunc)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rwetherall/memofunc?branch=master&svg=true)](https://ci.appveyor.com/project/rwetherall/memofunc)
[![Coveralls test coverage](https://coveralls.io/repos/github/rwetherall/memofunc/badge.svg)](https://coveralls.io/r/rwetherall/memofunc?branch=master)

## MemoFunc - A Function Memoization Package for R

A simple way to cache function results to improve performance by illiminating unessesary computation or data retrieval activities.

Functions can be memoized with a simple call to \code{memo}.

``` r

> # a simple example function
> simple.function <- function (value) {
+   print("Executing!")
+   value
+ }

> # call memo function to memoise a function
> simple.function.memo <- memo(simple.function)

> # or like this
> simple.function %<>% memo()

> # or use an anon function
> simple.function2 <- (function (value) value) %>% memo()

```
Calling a memo is exactly like calling a normal function, in fact it is a normal function!  The memo has all the same arguments and defaults as the origional function so it can be used in legacy code without the need for any risky refactoring.

Memoing a function can significantly improve the performance of a system by limiting how often expensive call are made.

``` r

> # the first time we call the memo the function will execute
> simple.function(10)
[1] "Executing!"
[1] 10

> # if we call the function again with the same parameter values then
> # the cached value will be returned
> simple.function(10)
[1] 10

> # calling the memo with a different set of parameter values will
> # cause the function to execute
> simple.function(20)
[1] "Executing!"
[1] 20

```

## Installation

``` r
devtools::install_github("rwetherall/memofunc")
```

