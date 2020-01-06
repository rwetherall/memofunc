context("hash")

library(magrittr)
library(testthat)

##
# Helper to test whether two functions are the same
#
expect_list_equal <- function (given, expected) {
  
  given %>% length() %>% expect_equal(length(expected))
  names(expected) %in% names(expected) %>% all() %>% expect_true()
  expected %in% given %>% all() %>% expect_true()
  
}

##
# Helper to test that we get the expected default arguments for a function
#
test_that_defaultArgs_expected <- function (fn.test, result.expected) {
  
  # get functions default arguments
  fn.test %>% formals() %>% defaultArgs() %>%
  
  # check we get the expected results
  expect_list_equal(result.expected)
  
}

test_that("
  Given some arguments, 
  When I ask which have not been named, 
  Then I get a list the missing names in the order they appear in the argument list", {
  
  test.fn <- function (a, b=10, c=10) NULL

  expect_list_equal(
    unused.formals(formals(test.fn), list(10, 10, 10)),
    list("a", "b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(a=10, 10, 10)),
    list("b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(10, 10, c=10)),
    list("a", "b"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(10, a=10, 10)),
    list("b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(b=10, a=10, c=10)),
    list())
})

test_that("
  Given a set of arguments with or without names, 
  When I ask for their names, 
  Then I get the full list of argument names in the order they appear", {
  
  test.fn <- function (a, b=10, c=10, d, e) NULL
  
  expect_list_equal(
    all.names(formals(test.fn), list(10, 10, 10, 10, 10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(a=10, 10, c=10, 10, 10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(a=10, b=10, c=10, d=10, e=10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(b=10, 10, d=10, 10, 10)),
    list("b", "a", "d", "c", "e"))
  
})


test_that("
  Given a function, 
  When I ask for the default values, 
  Then I get them", {
  
  # no default values
  test_that_defaultArgs_expected(
    function (value) NULL,
    alist()
  )
  
  # only default values
  test_that_defaultArgs_expected(
    function (def.value1 = "text", def.value2 = 10, def.value3 = list()) NULL,
    alist(def.value1 = "text", def.value2 = 10, def.value3 = list())
  )
  
  # mixed default and non-default values
  test_that_defaultArgs_expected(
    function (value1, def.value1 = 10, value2, value3, def.value2 = 20) NULL,
    alist(def.value1 = 10, def.value2 = 20)
  )
  
  # NULL, NA or empty default values
  test_that_defaultArgs_expected(
    function (def.value1 = NULL, def.value2 = "", def.value3 = NA) NULL,
    alist(def.value1 = NULL, def.value2 = "", def.value3 = NA)
  )
  
  # with elipse
  test_that_defaultArgs_expected(
    function (def.value1 = 10, ...) NULL,
    alist(def.value1 = 10)
  )
  
  # varible reference
  myValue <- "PEFW"
  test_that_defaultArgs_expected(
    function (def.value1 = myValue) NULL,
    alist(def.value1 = myValue)
  )
  
})

test_that("
  Given a list of default arguments, 
  When I inspect them against the provided arguments, 
  Then I will get the unset default values", {
  
  alist(y=20, z=20) %>% 
    unset.defaultArgs(alist(10, x=10, y=10)) %>%
    expect_list_equal(list(z=20)) 
})

test_that("
  Given a function and a call, 
  When I ask for the function call, 
  Then the arguments are fully named correctly", {

  test.fn <- function (a, b=10, c=10, d) NULL
  
  test.call1 <- call("test.fn", 20, b=30)
  test.call2 <- call("test.fn", 20, b=30, c=10)
  test.call3 <- call("test.fn", b=30, a=20)
  test.call4 <- call("test.fn", 20, 30)
  test.call5 <- call("test.fn", 20, 30, a=10, 40)
  
  with (functionCall(test.fn, test.call1), expect_list_equal(args, list(a=20, b=30)))
  with (functionCall(test.fn, test.call2), expect_list_equal(args, list(a=20, b=30, c=10)))
  with (functionCall(test.fn, test.call3), expect_list_equal(args, list(b=30, a=20)))
  with (functionCall(test.fn, test.call4), expect_list_equal(args, list(a=20, b=30)))
  with (functionCall(test.fn, test.call5), expect_list_equal(args, list(b=20, c=30, a=10, d=40)))
  
  
  
})

test_that("
  Given a function, 
  When I ask for the function call, 
  Then I get the arguments fully named 
  And the function 
  And the calling name", {
    
    test.fn <- function (a, b=10, c=10, d) functionCall()

    expect_equal(test.fn(20, 10)$f, test.fn)
    expect_equal(test.fn(20, 20)$name, quote(test.fn))
    
    with (test.fn(20, b=30), expect_list_equal(args, list(a=20, b=30)))
    with (test.fn(20, b=30, c=10), expect_list_equal(args, list(a=20, b=30, c=10)))
    with (test.fn(b=30, a=20), expect_list_equal(args, list(b=30, a=20)))
    with (test.fn(20, 30), expect_list_equal(args, list(a=20, b=30)))
    with (test.fn(20, 30, a=10, 40), expect_list_equal(args, list(b=20, c=30, a=10, d=40)))
  })

test_that("
  Given a function with mandatory and non-mandatory arguments, 
  When I hash the function call with the same argurment values in different order, 
  Then the result is always identical", {
  
  test.fn <- function (a, b=10, c=10) NULL
  
  test.call1 <- call("test.fn", 20, b=30)
  test.call2 <- call("test.fn", 20, b=30, c=10)
  test.call3 <- call("test.fn", b=30, a=20)
  test.call4 <- call("test.fn", 20, 30)
  test.call5 <- call("test.fn", 20, 30, 10)
  
  hash1 <- functionCall(test.fn, test.call1) %>% hash()
  hash2 <- functionCall(test.fn, test.call1) %>% hash()
  hash3 <- functionCall(test.fn, test.call2) %>% hash()
  hash4 <- functionCall(test.fn, test.call3) %>% hash()
  hash5 <- functionCall(test.fn, test.call4) %>% hash()
  hash6 <- functionCall(test.fn, test.call5) %>% hash()
  
  expect_equal(hash1, hash2)
  expect_equal(hash1, hash3)
  expect_equal(hash1, hash4)
  expect_equal(hash1, hash5)
  expect_equal(hash1, hash6)
  
})

## TODO Given a function with mandatory and non-mandatory arguments, When I hash the function call with different argument values, Then the results are different

## TODO Given a function with mandatory and non-mandatory arguments, When I hash the function call with varibale argument values, Then the results reflect the changes to the varible value


