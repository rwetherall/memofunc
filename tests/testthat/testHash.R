context("hash")

library(magrittr)
library(testthat)

test_that("Given a function with no default values, When I ask for the default values, Then I get none", {
  
  fn.test <- function (value) NULL
  result <- fn.test %>% formals() %>% defaultArgs()
  result %>% length() %>% expect_equal(0)
})

test_that("Given a function with only default values, When I ask for the default values, Then I get them", {
  
  fn.test <- function (def.value1 = "text", def.value2 = 10, def.value3 = list()) NULL
  
  result.expected <- c(def.value1 = "text", def.value2 = 10, def.value3 = list())
  result <- fn.test %>% formals() %>% defaultArgs()
  
  # check we get the expected results
  result %>% length() %>% expect_equal(3)
  names(result.expected) %in% names(result) %>% all() %>% expect_true()
  result.expected %in% result %>% all() %>% expect_true()
})