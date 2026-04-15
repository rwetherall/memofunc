[![CRAN status](https://www.r-pkg.org/badges/version/memofunc)](https://CRAN.R-project.org/package=memofunc)
[![R-CMD-check](https://github.com/rwetherall/memofunc/workflows/R-CMD-check/badge.svg)](https://github.com/rwetherall/memofunc/actions)
[![codecov](https://codecov.io/gh/rwetherall/memofunc/branch/master/graph/badge.svg?token=zPeCig27vf)](https://app.codecov.io/gh/rwetherall/memofunc)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rwetherall/memofunc?branch=master&svg=true)](https://ci.appveyor.com/project/rwetherall/memofunc)
[![](https://cranlogs.r-pkg.org/badges/memofunc)](https://cran.r-project.org/package=memofunc)

## MemoFunc - A Function Memoization Package for R

Programmable memory for R functions.

`memofunc` memoizes R functions with explicit, predictable cache behavior.
Function calls become reusable memory: repeated computations return quickly,
while cache keys remain transparent and deterministic.

## Why memofunc

- Deterministic keys: cached values are keyed by normalized call inputs and function
  identity (formals + body) by default.
- Pluggable storage: memory, file, and object storage providers.
- Runtime controls: force recomputation or dry-run cache checks without side effects.
- Safe evolution: implementation changes naturally invalidate stale values.
- Predictable behavior: clear options for NULL caching and key scoping via `id`.

## Installation

Install from CRAN:

```r
install.packages("memofunc")
```

Install latest release:

```r
devtools::install_github("rwetherall/memofunc")
```

## Quick Start

```r
library(memofunc)
library(magrittr)

slow_add <- function(a, b = 1) {
  Sys.sleep(1)
  a + b
}

memo_add <- memo(slow_add)

# first call executes
system.time(memo_add(10, 2))

# second call with same inputs hits cache
system.time(memo_add(10, 2))
```

## Memo Options and When to Use Them

- `id`: use to create an explicit cache namespace across wrappers/renames or generated functions.
- `allow.null = TRUE`: use when `NULL` is a valid return value and should be cached.
- `memo.force = TRUE` (call-time): use when you need to refresh a value now.
- `memo.dryrun = TRUE` (call-time): use to check whether execution would occur, without executing.
- `function_hash_override`: use only when you intentionally want function-identity changes to share the same key space.

## Practical Examples

### 1) Basic memoization

```r
simple_fn <- function(value) {
  print("Executing")
  value
}

simple_memo <- memo(simple_fn)
simple_memo(10)  # executes
simple_memo(10)  # cached
```

### 2) In-place memoization

```r
simple_fn %<>% memo()
simple_fn(10)
simple_fn(10)
```

### 3) Explicit id for anonymous/generated functions

```r
anon_memo <- memo(function(value) value * 2, id = "pricing/double")
anon_memo(5)
anon_memo(5)
```

### 4) Runtime controls (`memo.force`, `memo.dryrun`)

```r
simple_memo(30, memo.dryrun = TRUE)   # TRUE if not cached yet
simple_memo(30)                        # executes and caches
simple_memo(30, memo.dryrun = TRUE)   # FALSE because cached

simple_memo(30, memo.force = TRUE)     # forces recomputation
```

### 5) Caching NULL values

```r
returns_null <- memo(function(x) NULL)
returns_null(1)
returns_null(1)  # executes again because NULL is not cached by default

returns_null_cached <- memo(function(x) NULL, allow.null = TRUE)
returns_null_cached(1)
returns_null_cached(1)  # cached
```

### 6) Stable keys across implementation changes (advanced)

```r
# to share cache across memo instances, use persistent storage
base_dir <- file.path(tempdir(), "memofunc-readme-function-hash")
unlink(base_dir, recursive = TRUE, force = TRUE)

old_opts <- options(memofunc.storage.provider = list(name = "file", base.dir = base_dir))
on.exit(options(old_opts), add = TRUE)
on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

counter <- new.env(parent = emptyenv())
counter$n <- 0

f_v1 <- function(value) {
  counter$n <- counter$n + 1
  value
}

f_v2 <- function(value) {
  counter$n <- counter$n + 1
  value + 1
}

# default behavior: changed implementation gets a different function component
memo(f_v1, id = "shared-id")(10)  # 10 (executes)
counter$n <- 0
memo(f_v2, id = "shared-id")(10)  # 11 (executes, no cache hit from v1)
counter$n                          # 1

# override behavior: force both implementations to share function component
memo(f_v1, id = "shared-id", function_hash_override = "stable-v1")(20)  # 20 (executes)
counter$n <- 0
memo(f_v2, id = "shared-id", function_hash_override = "stable-v1")(20)  # 20 (cache hit from v1)
counter$n                                                                    # 0 (v2 did not execute)

# note: this reuse works because both calls share the same file-backed storage.
# with separate in-memory memo instances, keys may match but values are not shared.
```

## Hashing Strategy (How Values Are Stored)

Each memoized call is stored against a final key that combines three hashed components:

1. `id` component
   - explicit `id` when supplied
   - otherwise inferred function name when available
2. function component
   - default: hash of function formals + body
   - optional override: hash of `function_hash_override`
3. call component
   - normalized argument values for the specific call
   - includes defaulted parameters
   - named arguments are order-insensitive (`f(a = 1, b = 2)` equals `f(b = 2, a = 1)`)

Conceptually:

```text
final_key = H(
  id_component,
  function_component,
  call_component
)
```

Important: key equality and storage equality are separate concerns.

- Matching keys are necessary to reuse a value.
- Memo instances must also read/write the same underlying store.
- Default in-memory memo instances have separate stores.
- Shared file/object storage allows separate memo instances to reuse matching keys.

Implications:

- Function changes naturally invalidate future reads from older entries by default.
- Historical entries remain in storage until explicitly cleared.
- `function_hash_override` should be used deliberately, because it can keep cache continuity
  across implementation changes.

## Storage Backends

By default, memoized values are stored in memory. You can opt into file or object storage.

```r
# direct local file storage
file_storage <- storage.init("file", base.dir = file.path(tempdir(), "memofunc"))

# provider-backed object storage using local files
object_storage <- storage.init("object", provider = "file", base.dir = file.path(tempdir(), "memofunc"))

# set default provider globally
options(memofunc.storage.provider = list(name = "file", base.dir = file.path(tempdir(), "memofunc")))
storage.init()
```

Azure Blob example (requires `AzureStor` and credentials):

```r
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

