
context("memoFunction")

library(testthat)
library(memofunc)

test1 <- function(value) {
  Sys.setenv(executed = TRUE)
  value
}

call_memoed_fn <- function(f, value, force = FALSE) {
  Sys.setenv(executed=FALSE)
  list(
    value = f(value, force=force),
    executed = as.logical(Sys.getenv("executed")))
}

test_that("Given a simple function, When I memoise the function, Then the results are cached",{

  # test function
  test1.memo <- memo(test1)

  # initial call executes method
  call1 <- call_memoed_fn(test1.memo, 10)
  expect_equal(call1$value, 10)
  expect_true(call1$executed)

  # second call with same params returns cached value
  call2 <- call_memoed_fn(test1.memo, 10)
  expect_equal(call2$value, 10)
  expect_false(call2$executed)

  # force call executes method
  call3 <- call_memoed_fn(test1.memo, 10, TRUE)
  expect_equal(call3$value, 10)
  expect_true(call3$executed)

  # call with different params executes method

})
