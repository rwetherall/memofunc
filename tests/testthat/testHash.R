context("hash")

library(magrittr)
library(testthat)

test_that_defaultArgs_expected <- function (fn.test, result.expected) {
  
  # get functions default arguments
  result <- fn.test %>% formals() %>% defaultArgs()
  
  # check we get the expected results
  result %>% length() %>% expect_equal(length(result.expected))
  names(result.expected) %in% names(result) %>% all() %>% expect_true()
  result.expected %in% result %>% all() %>% expect_true()
  
}

test_that("Given a function, When I ask for the default values, Then I get them", {
  
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
