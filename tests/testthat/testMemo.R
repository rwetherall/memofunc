
context("memo")

library(magrittr)
library(testthat)

# environment used to mark memo execution
current.test.env <- function () test.env

# memo execution key
key.executed <- "executed"

##
# Helper to insert expression into function to be executed before the current body.
#
insert.before <- function (f, expr) {
  
  expr.rest <- function (expr) {
    
    expr.list <- expr %>% as.list()
    
    if (length(expr.list) == 1) expr.list else rest(expr.list)
  }
  
  body(f) <- c(`{`, expr.rest(expr), expr.rest(body(f))) %>% as.call()
  
  f
}

##
# marks memo execution in environment
#
mark.executed <- function () assign(key.executed, TRUE, envir=test.env)

## 
# inserts code to mark environment with execution flag before memoising the function
#
test.memo <- function (f, ...) f %>% insert.before(quote({mark.executed()})) %>% memo(...)

##
# do memo test
#
do.test <- function (f, params, expected, executed) {

  assign("test.env", test_env(), envir = environment(current.test.env))
  expect_true(identical(do.call(f, params), expected))
  expect_equal(mget(key.executed, envir=current.test.env(), inherits=FALSE, ifnotfound=FALSE)[[1]], executed)
}

test_that("
  Given a simple function which has been memoised, 
  When I evaluate the memo, 
  Then the result is cached for the same parameters after the first call", {
  
  memo <- (function (value) value) %>% test.memo()
  
  do.test(memo, list(10), 10, TRUE)
  do.test(memo, list(10), 10, FALSE)
  do.test(memo, list(10), 10, FALSE)
  do.test(memo, list(20), 20, TRUE)
  do.test(memo, list(20), 20, FALSE)
})

test_that("
  Given a simple function which has been memoised,
  When I evaluate the memo and specifiy the force parameter,
  Then the memo is executed if force is TRUE
  And the new value is cached", {
  
  memo <- (function (value) value) %>% test.memo()
  
  do.test(memo, list(10, memo.force=FALSE), 10, TRUE)
  do.test(memo, list(10, memo.force=TRUE), 10, TRUE)
  do.test(memo, list(10, memo.force=FALSE), 10, FALSE)
  do.test(memo, list(10, memo.force=TRUE), 10, TRUE)
})

test_that("
  Given a simple function that has no return value
  And has been memoised with the default arguments,
  When I evaluate the memo,
  Then it will always execute", {
    
  memo <- (function (value) return(NULL)) %>% test.memo()
  
  do.test(memo, list(10), NULL, TRUE)
  do.test(memo, list(10), NULL, TRUE)
  do.test(memo, list(20), NULL, TRUE)
  do.test(memo, list(10, memo.force=TRUE), NULL, TRUE)
  
  memo <- (function (value) return(NULL)) %>% test.memo(allow.null=FALSE)
  
  do.test(memo, list(10), NULL, TRUE)
  do.test(memo, list(10), NULL, TRUE)
  do.test(memo, list(20), NULL, TRUE)
  do.test(memo, list(20), NULL, TRUE)
  do.test(memo, list(10, memo.force=TRUE), NULL, TRUE)
})

test_that("
  Given a simple function that has no arguments,
  And has been memoised,
  When I evaluate the memo,
  Then it will cache the result as expected", {
    
  memo <- (function () 10) %>% test.memo()
  
  do.test(memo, list(), 10, TRUE)
  do.test(memo, list(), 10, FALSE)
})

test_that("
  Given a simple function that has no return value
  And has been memoised indicating that null results are allowed,
  When I evaluate the memo,
  Then it will cache NULL results as normal", {
    
  memo <- (function (value) return(NULL)) %>% test.memo(allow.null=TRUE)
  
  do.test(memo, list(10), NULL, TRUE)
  do.test(memo, list(10), NULL, FALSE)
  do.test(memo, list(20), NULL, TRUE)
  do.test(memo, list(20), NULL, FALSE)
  do.test(memo, list(10, memo.force=TRUE), NULL, TRUE)
})

## TODO what happens if the function returns an invisible value
## TODO what happens if the function returns NA or "" ??

## TODO show that different memos do not share cached values

test_that("
  Given a memo,
  When I ask for the cache,
  Then I get the cache", {
    
  memo <- (function (value) value) %>% memo() 
  memo %>% memo.cache() %>% is.null() %>% expect_false()
})

test_that("
  Given a memo,
  When I ask for the function,
  Then I get the original function", {
    
  memo <- (function (value) value) %>% memo() 
  memo %>% memo.function() %>% hash() %>% expect_equal(hash(function (value) value))
})

test_that("
  Given a memo,
  When I memo the memo,
  Then I get an error", {
    
  expect_error((function (value) value) %>% memo() %>% memo())
})

test_that("
  Given a memo,
  When I call it with the dry run argument set to TRUE,
  Then it returns TRUE if the memoed function would be executed and FALSE if the value would have been 
  retrived from the cache,
  And it doesn't store these values in the cache", {

  memo <- (function (value) value) %>% memo()
  
  memo(10, memo.dryrun = TRUE) %>% expect_true()
  memo(10, memo.dryrun = TRUE) %>% expect_true()
  memo(10, memo.dryrun = FALSE) %>% expect_equal(10)
  memo(10, memo.dryrun = TRUE) %>% expect_false()
  memo(10, memo.force = TRUE, memo.dryrun = TRUE) %>% expect_true()
  memo(10, memo.dryrun = FALSE) %>% expect_equal(10)
  memo(10, memo.force = TRUE, memo.dryrun = FALSE) %>% expect_equal(10)
  memo(10) %>% expect_equal(10)
  
})

test_that("
  Given a named function,
  When I memoise without an explicit id,
  Then the id defaults to the function name", {

  named.fn <- function (value) value
  memoed <- memo(named.fn)

  expect_equal(attr(memoed, "memo.id"), "named.fn")
})

test_that("
  Given a function and explicit id,
  When I memoise with id,
  Then the id is stored on the memo", {

  memoed <- memo(function (value) value, id = "explicit-id")

  expect_equal(attr(memoed, "memo.id"), "explicit-id")
})

test_that("
  Given a persistent cache and explicit id,
  When the underlying function body changes,
  Then previous cache entries are ignored for the new memo function hash", {

  base.dir <- file.path(tempdir(), "memofunc-test-versioned")
  unlink(base.dir, recursive = TRUE, force = TRUE)

  old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
  on.exit(options(old.options), add = TRUE)
  on.exit(unlink(base.dir, recursive = TRUE, force = TRUE), add = TRUE)

  counter <- new.env(parent = emptyenv())
  counter$executed <- 0

  v1 <- function (value) {
    counter$executed <- counter$executed + 1
    value
  }

  m1 <- memo(v1, id = "shared-id")
  expect_equal(m1(10), 10)
  expect_equal(counter$executed, 1)

  counter$executed <- 0

  v2 <- function (value) {
    counter$executed <- counter$executed + 1
    value + 1
  }

  m2 <- memo(v2, id = "shared-id")
  expect_equal(m2(10), 11)
  expect_equal(counter$executed, 1)
  expect_equal(m2(10), 11)
  expect_equal(counter$executed, 1)
})

test_that("
  Given an explicit id and explicit function hash override,
  When the function body changes,
  Then the function hash override keeps reading existing cache entries", {

  base.dir <- file.path(tempdir(), "memofunc-test-function-hash-override")
  unlink(base.dir, recursive = TRUE, force = TRUE)

  old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
  on.exit(options(old.options), add = TRUE)
  on.exit(unlink(base.dir, recursive = TRUE, force = TRUE), add = TRUE)

  counter <- new.env(parent = emptyenv())
  counter$executed <- 0

  v1 <- function (value) {
    counter$executed <- counter$executed + 1
    value
  }

  m1 <- memo(v1, id = "shared-id", function_hash_override = "stable-v1")
  expect_equal(m1(10), 10)
  expect_equal(counter$executed, 1)

  counter$executed <- 0

  v2 <- function (value) {
    counter$executed <- counter$executed + 1
    value + 1
  }

  m2 <- memo(v2, id = "shared-id", function_hash_override = "stable-v1")
  expect_equal(m2(10), 10)
  expect_equal(counter$executed, 0)
})

test_that("
  Given no explicit id,
  When a function hash override is provided,
  Then it is accepted and used for memoisation", {

  named.fn <- function (value) value
  memoed <- memo(named.fn, function_hash_override = "stable-v1")

  expect_equal(attr(memoed, "memo.id"), "named.fn")
  expect_equal(attr(memoed, "memo.function_hash"), hash("stable-v1"))
})

test_that("
  Given a memoized function with shared storage,
  When I invalidate the memo,
  Then only that memo's cached values are removed", {

  base.dir <- file.path(tempdir(), "memofunc-test-invalidate-all")
  unlink(base.dir, recursive = TRUE, force = TRUE)

  old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
  on.exit(options(old.options), add = TRUE)
  on.exit(unlink(base.dir, recursive = TRUE, force = TRUE), add = TRUE)

  counter.a <- new.env(parent = emptyenv())
  counter.a$executed <- 0
  counter.b <- new.env(parent = emptyenv())
  counter.b$executed <- 0

  memo.a <- memo(function (value) {
    counter.a$executed <- counter.a$executed + 1
    value
  }, id = "memo-a")

  memo.b <- memo(function (value) {
    counter.b$executed <- counter.b$executed + 1
    value
  }, id = "memo-b")

  memo.a(10)
  memo.b(10)

  counter.a$executed <- 0
  counter.b$executed <- 0

  invalidate(memo.a)

  memo.a(10)
  memo.b(10)

  expect_equal(counter.a$executed, 1)
  expect_equal(counter.b$executed, 0)
})

test_that("
  Given a memoized function,
  When I invalidate by argument list,
  Then only that cached call is removed", {

  base.dir <- file.path(tempdir(), "memofunc-test-invalidate-key")
  unlink(base.dir, recursive = TRUE, force = TRUE)

  old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
  on.exit(options(old.options), add = TRUE)
  on.exit(unlink(base.dir, recursive = TRUE, force = TRUE), add = TRUE)

  counter <- new.env(parent = emptyenv())
  counter$executed <- 0

  memoed <- memo(function (value) {
    counter$executed <- counter$executed + 1
    value
  }, id = "memo-invalidate-key")

  memoed(1)
  memoed(2)

  counter$executed <- 0
  invalidate(memoed, list(value = 1))

  memoed(1)
  memoed(2)

  expect_equal(counter$executed, 1)
})

test_that("
  Given a memoized function,
  When I invalidate by explicit storage key,
  Then the selected cache entry is removed", {

  base.dir <- file.path(tempdir(), "memofunc-test-invalidate-storage-key")
  unlink(base.dir, recursive = TRUE, force = TRUE)

  old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
  on.exit(options(old.options), add = TRUE)
  on.exit(unlink(base.dir, recursive = TRUE, force = TRUE), add = TRUE)

  counter <- new.env(parent = emptyenv())
  counter$executed <- 0

  memoed <- memo(function (value) {
    counter$executed <- counter$executed + 1
    value
  }, id = "memo-invalidate-storage-key")

  memoed(1)

  key <- memo.storage_key(
    id = attr(memoed, "memo.id"),
    function_hash = attr(memoed, "memo.function_hash"),
    call_hash = memo.call_hash(memo.function(memoed), list(value = 1))
  )

  counter$executed <- 0
  invalidate(memoed, key)
  memoed(1)

  expect_equal(counter$executed, 1)
})

test_that("
  Given a non-memo function,
  When I call invalidate,
  Then it errors", {

  expect_error(invalidate(function (value) value))
})
