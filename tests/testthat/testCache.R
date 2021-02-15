context("cache")

library(uuid)
library(testthat)

test_that("Given the default cache settings, When I access the cache for the first time, Then expected cache functions are available", {

  # create memory cache
  cache <- cache()
  
  # check the details of the cache
  expect_equal(class(cache), "cache")
  expect_false(is.null(cache$memory_cache))
  expect_equal(class(cache$memory_cache), "environment")
  expect_false(is.null(cache$storage))
  expect_equal(class(cache$storage), "memory")

})

test_that("Given the default cache settings, When a value is set, Then it can be retrieved", {

  # create memory cache
  cache <- cache()

  expect_equal(cache.set(cache, "string", "one") %>% cache.get("string"), "one")
  expect_equal(cache.set(cache, "number", 100) %>% cache.get("number"), 100)
  expect_equal(cache.set(cache, "logical", TRUE) %>% cache.get("logical"), TRUE)

  myTestFn <- function() print("Hello")
  expect_equal(cache.set(cache, "function", myTestFn) %>% cache.get("function"), myTestFn)
})

test_that("Given the default cache settings, When retieving a value that hasn't been set, Then NULL is retrieved", {

  expect_null(cache.get(cache(), "value"))
})

test_that("Given the default cache settings, When a value is unset, Then it is no longer in the cache", {

  # create memory cache
  cache <- cache()

  expect_equal(cache.set(cache, "value", "one") %>% cache.get("value"), "one")
  cache.unset(cache, "value")
  expect_null(cache.get(cache, "value"))

  # shouldn't issue warning
  cache.unset(cache, "value")
})

test_that("Given the default cache settings, when the cache is clear, Then it no longer exists", {

  # create memory cache
  cache <- cache()

  cache.set(cache, "value","one")
  cache.set(cache, "valuetoo", "two")
  expect_equal(cache.get(cache, "value"), "one")
  expect_equal(cache.get(cache, "valuetoo"), "two")

  cache.clear(cache)
  expect_null(cache.get(cache, "value"))
  expect_null(cache.get(cache, "valuetoo"))
})

test_that("Given the default cache settings, When the a cache value is reset, Then the new value is taken into the cache", {

  # create memory cache
  cache <- cache()

  cache.set(cache, "value", "one")
  expect_equal(cache.get(cache, "value"), "one")
  cache.set(cache, "value", "two")
  expect_equal(cache.get(cache, "value"), "two")

})

test_that("Given the default cache settings, When I ask, Then I can determine whether the cache contains a key value or not", {

  # create memory cache
  cache <- cache()

  expect_false(cache.has(cache, "value"))
  cache.set(cache, "value", "one")
  expect_true(cache.has(cache, "value"))

})

test_that("Given I have two caches with different names, When I work with the caches, Then values are stored separately", {

  # create memory cache
  cache <- cache()
  cache2 <- cache()

  cache.set(cache, "default", "one")
  cache.set(cache2, "another", "one")

  expect_true(cache.has(cache, "default"))
  expect_false(cache.has(cache, "another"))

  expect_false(cache.has(cache2, "default"))
  expect_true(cache.has(cache2, "another"))
})

test_that("Given that I have a cache, When I store a key with a NULL value, Then I can retrive and check for existance successfully", {
  
  cache <- cache()
  
  cache.set(cache, "nullvalue", NULL);
  
  expect_true(cache.has(cache, "nullvalue"))
  expect_null(cache.get(cache, "nullvalue"))
  
})

# TODO check valid algo

# TODO check invalid algo

