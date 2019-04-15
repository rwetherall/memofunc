[![Build Status](https://travis-ci.com/rwetherall/memofunc.svg?token=x2QLvsytRz6d82hRES7c&branch=master)](https://travis-ci.com/rwetherall/memofunc)
[![codecov](https://codecov.io/gh/rwetherall/memofunc/branch/master/graph/badge.svg?token=zPeCig27vf)](https://codecov.io/gh/rwetherall/memofunc)

## MemoFunc - A Function Memoization Package for R

MemoFunc is a function memoization package for R.  

It provides a simple way to cache function results to improve performance by illiminating unessesary computation or data retrieval activities.

``` r
myMemoedFn <- memo(myFn, cache())
```

MemoFunc also provides a general purpose cacheing capability which can be easily extended with additional storage mediums and strategies.

``` r
library(memofunc)

cache <- cache("memory")

cache$set("remember", "this")
cache$get("remember")

```

## Installation

``` r
devtools::install_github("rwetherall/memofunc")
```

## Usage

``` r
library(memofunc)
```
