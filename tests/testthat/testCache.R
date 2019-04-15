context("cache")

library(testthat)

test_that("Given the default cache settings, When I access the cache for the first time, Then expected cache functions are available", {

  # create memory cache
  cache <- cache()

  # check that the expected 'methods' are avilable on the cache
  expect_true(is.function(cache$set))
  expect_true(is.function(cache$get))
  expect_true(is.function(cache$unset))
  expect_true(is.function(cache$has))
  expect_true(is.function(cache$clear))
  expect_true(is.function(cache$ls))
})

test_that("Given the default cache settings, When a value is set, Then it can be retrieved", {

  # create memory cache
  cache <- cache()

  expect_equal(cache$set("string", "one"), "one")
  expect_equal(cache$get("string"), "one")

  expect_equal(cache$set("number", 100), 100)
  expect_equal(cache$get("number"), 100)

  expect_equal(cache$set("logical", TRUE), TRUE)
  expect_equal(cache$get("logical"), TRUE)

  myTestFn <- function() print("Hello")
  expect_equal(cache$set("function", myTestFn), myTestFn)
  expect_equal(cache$get("function"), myTestFn)
})

test_that("Given the default cache settings, When retieving a value that hasn't been set, Then NULL is retrieved", {

  expect_null(cache()$get("value"))
})

test_that("Given the default cache settings, When a value is unset, Then it is no longer in the cache", {

  # create memory cache
  cache <- cache()

  expect_equal(cache$set("value", "one"), "one")
  expect_equal(cache$get("value"), "one")
  cache$unset("value")
  expect_null(cache$get("value"))

  # shouldn't issue warning
  cache$unset("value")
})

test_that("Given the default cache settings, when the cache is clear, Then it no longer exists", {

  # create memory cache
  cache <- cache()

  cache$set("value","one")
  cache$set("valuetoo", "two")
  expect_equal(cache$get("value"), "one")
  expect_equal(cache$get("valuetoo"), "two")

  cache$clear()
  expect_null(cache$get("value"))
  expect_null(cache$get("valuetoo"))
})

test_that("Given the default cache settings, When the a cache value is reset, Then the new value is taken into the cache", {

  # create memory cache
  cache <- cache()

  cache$set("value", "one")
  expect_equal(cache$get("value"), "one")
  cache$set("value", "two")
  expect_equal(cache$get("value"), "two")

})

test_that("Given the default cache settings, When I ask, Then I can determine whether the cache contains a key value or not", {

  # create memory cache
  cache <- cache()

  expect_false(cache$has("value"))
  cache$set("value", "one")
  expect_true(cache$has("value"))

})

test_that("Given I have two caches with different names, When I work with the caches, Then values are stored separately", {

  # create memory cache
  cache <- cache()
  cache2 <- cache()

  cache$set("default", "one")
  cache2$set("another", "one")

  expect_true(cache$has("default"))
  expect_false(cache$has("another"))

  expect_false(cache2$has("default"))
  expect_true(cache2$has("another"))
})


# TODO check that by default a cache with memory storage is created

# TODO check valid algo

# TODO check invalid algo

