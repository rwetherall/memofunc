
context("memoFunction")

library(testthat)

test.env <- new.env(parent=emptyenv())

test1 <- function(value) {
  assign("test1.executed", TRUE, envir=test.env)
  value
}

test_that("Given a simple function, When I memoise the function, Then the results are cached",{

  # test function
  test1.memo <- memo(test1, cache=cache())

  # initial call executes method
  expect_equal(test1.memo(10), 10)
  expect_equal(get("test1.executed",envir=test.env), TRUE)
  rm(list=ls(test.env), envir=test.env)

  # second call with same params returns cached value
  expect_equal(test1.memo(10), 10)
  expect_error(get("test1.executed",envir=test.env))
  rm(list=ls(test.env), envir=test.env)

  # a call with force false doesn't execute
  expect_equal(test1.memo(10, force=FALSE), 10)
  expect_error(get("test1.executed",envir=test.env))
  rm(list=ls(test.env), envir=test.env)

  # force call executes method
  expect_equal(test1.memo(10, force=TRUE), 10)
  expect_equal(get("test1.executed",envir=test.env), TRUE)
  rm(list=ls(test.env), envir=test.env)

  # call with different params executes method
  expect_equal(test1.memo(20), 20)
  expect_equal(get("test1.executed",envir=test.env), TRUE)
  rm(list=ls(test.env), envir=test.env)

})

# TODO Given a memo'ed function make sure it can still access the hierarchy of environments correctly ..
#      for example global variables .. what if it's a closure?

