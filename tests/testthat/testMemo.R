
context("memo")

library(magrittr)
library(testthat)

test.env <- NULL
key.executed <- "executed"

do.test <- function (f, params, expected, executed) {

  test.env <<- test_env()
  expect_equal(do.call(f, params), expected)
  expect_equal(mget(key.executed, envir=test.env, inherits=FALSE, ifnotfound=FALSE)[[1]], executed)
}

test_that("
  Given a simple function which has been memo'ised, 
  When I evaluate the memo, 
  Then the result is cached for the same parameters after the first call", {
  
  test <- function (value) {
    assign(key.executed, TRUE, envir=test.env)
    value
  }
  
  memo <- test %>% memo()
  
  do.test(memo, list(10), 10, TRUE)
  do.test(memo, list(10), 10, FALSE)
  do.test(memo, list(10), 10, FALSE)
  do.test(memo, list(20), 20, TRUE)
  do.test(memo, list(20), 20, FALSE)
})