[![Build Status](https://travis-ci.com/rwetherall/memofunc.svg?token=x2QLvsytRz6d82hRES7c&branch=master)](https://travis-ci.com/rwetherall/memofunc)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rwetherall/memofunc?branch=master&svg=true)](https://ci.appveyor.com/project/rwetherall/memofunc)
[![Coveralls test coverage](https://coveralls.io/repos/github/rwetherall/memofunc/badge.svg)](https://coveralls.io/r/rwetherall/memofunc?branch=master)

## MemoFunc - A Function Memoization Package for R

This package provides a simple way to cache function results to improve performance by illiminating unessesary computation or data retrieval activities.

Functions can be memoized with a simple call to the memo function.

``` r

> library(memofunc)

> double <- function (value) value*2

> memoedDouble <- memo(double)

```
A memoed function can be called as normal with the same parameters as the original.

``` r

> memoedDouble(10)
[1] 20

```

The first time the function is called it is executed and the result cached.  Subsequent calls will return the cached value without executing the function logic, thus saving time.  The cached results are keyed on the passed parameter values.

## Installation

``` r
devtools::install_github("rwetherall/memofunc")
```

## Usage

### Forcing Execution

Execution of a memoed function can be forced in situations where it is known that that cached data may be out of date by using the force parameter.

``` r

> memoedDouble(10, force=TRUE)
[1] 20

```
Note that useing force=TRUE will not only force the function implementation to be executed even if a cached value is available, but it will also overwirte any exisiting cache values with the new one so it is available for future calls to the memoed function.

### Identifing a Memoed Function

Memoed functions can be identified using the is.memo function.

``` r

> is.memo(double)
[1] FALSE

> is.memo(memoedDouble)
[1] TRUE

```

### Accessing the Original Function

Sometimes it might be desirable to access and call the original function.  This can be achieved using the memo.function function.

``` r

> ogFunction <- memo.function(memoedDouble)

> ogFunction(20)
[1] 40

```

### Managing the Cache of a Memoed Function

Behind every memoed function is a simple cache.  You can access this cache using the memo.cache function.

``` r

> cache <- memo.cache(memoedDouble)

```

This allows the cache to be managed directly.  In perticular the contents of the cache can be inspected, retrieved, updated and cleared.

``` r

> cache$ls()
[1] "47c584c88970ee81c783da03093871ea"

> cache$get("47c584c88970ee81c783da03093871ea")
[1] 20

> memoedDouble(10)
[1] 20

> cache$set("47c584c88970ee81c783da03093871ea", 100)
[1] 100

> memoedDouble(10)
[1] 100

> cache$clear()

> cache$ls()
character(0)

> memoedDouble(10)
[1] 20

```

