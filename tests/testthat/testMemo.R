
context("memo")

library(magrittr)
library(testthat)

# environment used to mark memo execution
test.env <- NULL

# memo execution key
key.executed <- "executed"

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

  test.env <<- test_env()
  expect_true(identical(do.call(f, params), expected))
  expect_equal(mget(key.executed, envir=test.env, inherits=FALSE, ifnotfound=FALSE)[[1]], executed)
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
