[![CRAN status](https://www.r-pkg.org/badges/version/memofunc)](https://CRAN.R-project.org/package=memofunc)
[![R-CMD-check](https://github.com/rwetherall/memofunc/workflows/R-CMD-check/badge.svg)](https://github.com/rwetherall/memofunc/actions)
[![codecov](https://codecov.io/gh/rwetherall/memofunc/branch/master/graph/badge.svg?token=zPeCig27vf)](https://codecov.io/gh/rwetherall/memofunc)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rwetherall/memofunc?branch=master&svg=true)](https://ci.appveyor.com/project/rwetherall/memofunc)
[![](https://cranlogs.r-pkg.org/badges/memofunc)](https://cran.r-project.org/package=memofunc)

## MemoFunc - A Function Memoization Package for R

Programmable memory for R functions.

memofunc extends traditional memoization by treating function calls as first-class memory primitives. Instead of simple input → output caching, it gives functions memory you can persist, inspect, and compose across functions.

## Why memofunc

Traditional memoization focuses on deterministic caching. memofunc focuses on the memory of function calls:

- Function identity: a call is tied to the function signature and implementation, not just the input.
- Persistent memory: store results beyond a single session with pluggable storage backends.
- Inspectable history: trace what was computed, when, and why.
- Composable memory: reuse and share cached results across functions.

Compared to traditional memoization, memofunc treats cached results as durable, inspectable memory that can be composed across functions and storage backends.

Functions can be memoized with a simple call to memo.

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

> # or like this
> simple.function2 <- (function (value) value) %>% memo()

```
Calling a memo is exactly like calling a normal function, in fact it is a normal function!  The memo has all the same arguments and defaults as the origional function so it can be used in legacy code without the need for any risky refactoring.

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

Memoing a function can significantly improve the performance of a system by limiting how often expensive call are made.  Functions that return a NULL value can be memoed by using the allow.null argument.

``` r

> # consider a slow function which is memoised, note that we have used the allow.null argument
> # so that NULL is cached when returned from a function, the default is FALSE
> slow.function <- (function (value) Sys.sleep(value)) %>% memo(allow.null = TRUE)

> # the first time we call the slow function it takes some time
> system.time(slow.function(3))
   user  system elapsed 
   0.00    0.00    3.01 

> # subsequent calls make use of the cache and are much faster
> system.time(slow.function(3))
   user  system elapsed 
   0.01    0.00    0.02 

```

## Storage backends

By default, memoised values are stored in memory. You can opt into file or object storage backends.

``` r
# local file storage
file.storage <- storage.init("file", base.dir = file.path(tempdir(), "memofunc"))

# object storage using a provider name
object.storage <- storage.init("object", provider = "file", base.dir = file.path(tempdir(), "memofunc"))

# default provider via options
options(memofunc.storage.provider = list(name = "file", base.dir = file.path(tempdir(), "memofunc")))
storage.init()

# Azure Blob provider (requires AzureStor + credentials)
if (requireNamespace("AzureStor", quietly = TRUE)) {
  account <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
  container <- Sys.getenv("AZURE_STORAGE_CONTAINER")
  key <- Sys.getenv("AZURE_STORAGE_KEY")
  token <- Sys.getenv("AZURE_STORAGE_TOKEN")

  if (account != "" && container != "" && (key != "" || token != "")) {
    storage.init(
      "object",
      provider = "azure.blob",
      account = account,
      container = container,
      key = if (key == "") NULL else key,
      token = if (token == "") NULL else token,
      prefix = "memofunc"
    )
  }
}
```

## Installation

Install from CRAN:

```r
install.packages("memofunc")
```

Install latest release:

``` r
devtools::install_github("rwetherall/memofunc")
```

