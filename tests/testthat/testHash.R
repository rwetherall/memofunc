context("hash")

library(magrittr)
library(testthat)

expect_list_equal <- function (given, expected) {
  
  given %>% length() %>% expect_equal(length(expected))
  names(expected) %in% names(expected) %>% all() %>% expect_true()
  expected %in% given %>% all() %>% expect_true()
  
}

test_that_defaultArgs_expected <- function (fn.test, result.expected) {
  
  # get functions default arguments
  fn.test %>% formals() %>% defaultArgs() %>%
  
  # check we get the expected results
  expect_list_equal(result.expected)
  
}

test_that(
  "Given a function, 
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

test_that(
  "Given a list of default arguments, 
   When I inspect them against the provided arguments, 
   Then I will get the unset default values", {
  
  alist(y=20, z=20) %>% 
    unset.defaultArgs(alist(10, x=10, y=10)) %>%
    expect_list_equal(list(z=20))
})

test_that("", {
  
  test.fn <- function (a, b=10, c=10) NULL
  
  test.call1 <- call("test.fn", 20, b=30)
  test.call2 <- call("test.fn", 20, b=30, c=10)
  test.call3 <- call("test.fn", b=30, a=20)
  test.call4 <- call("test.fn", 20, 30)
  
  hash1 <- functionCall(test.fn, test.call1) %>% hash()
  hash2 <- functionCall(test.fn, test.call1) %>% hash()
  hash3 <- functionCall(test.fn, test.call2) %>% hash()
  hash4 <- functionCall(test.fn, test.call3) %>% hash()
  hash5 <- functionCall(test.fn, test.call4) %>% hash()
  
  expect_equal(hash1, hash2)
  expect_equal(hash1, hash3)
  expect_equal(hash1, hash4)
  expect_equal(hash1, hash5)
  
})
