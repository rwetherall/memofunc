context("memoryMemoCache")

library(testthat)

setup(cacheRegistry <<- list())

#
# Custome test method to initialise and clean up cache.
#
test_that_cache <- function(desc, code) {

  initCache()
  test_that(desc, code)
  cacheRegistry <<- list()
}

test_that_cache("Given the default cache settings, When I access the cache for the first time, Then expected cache functions are available", {

  # assess the default memory cache
  result <- getCache()

  # check that the expected 'methods' are avilable on the cache
  expect_true(is.function(result$set))
  expect_true(is.function(result$get))
  expect_true(is.function(result$unset))
  expect_true(is.function(result$has))
  expect_true(is.function(result$clear))
})


test_that_cache("Given the default cache settings, When a value is set, Then it can be retrieved", {

  expect_equal(getCache()$set("string", "one"), "one")
  expect_equal(getCache()$get("string"), "one")

  expect_equal(getCache()$set("number", 100), 100)
  expect_equal(getCache()$get("number"), 100)

  myMemoCache <- getCache()
  expect_equal(myMemoCache$set("logical", TRUE), TRUE)
  expect_equal(myMemoCache$get("logical"), TRUE)

  myTestFn <- function() print("Hello")
  expect_equal(getCache()$set("function", myTestFn), myTestFn)
  expect_equal(myMemoCache$get("function"), myTestFn)
})

test_that_cache("Given the default cache settings, When retieving a value that hasn't been set, Then NULL is retrieved", {

  expect_null(getCache()$get("value"))
})

test_that_cache("Given the default cache settings, When a value is unset, Then it is no longer in the cache", {

  expect_equal(getCache()$set("value", "one"), "one")
  expect_equal(getCache()$get("value"), "one")
  getCache()$unset("value")
  expect_null(getCache()$get("value"))
})

test_that_cache("Given the default cache settings, when the cache is clear, Then it no longer exists", {

  getCache()$set("value","one")
  getCache()$set("valuetoo", "two")
  expect_equal(getCache()$get("value"), "one")
  expect_equal(getCache()$get("valuetoo"), "two")

  getCache()$clear()
  expect_null(getCache()$get("value"))
  expect_null(getCache()$get("valuetoo"))
})

test_that_cache("Given the default cache settings, When the a cache value is reset, Then the new value is taken into the cache", {

  getCache()$set("value", "one")
  expect_equal(getCache()$get("value"), "one")
  getCache()$set("value", "two")
  expect_equal(getCache()$get("value"), "two")

})

test_that_cache("Given the default cache settings, When I ask, Then I can determine whether the cache contains a key value or not", {

  expect_false(getCache()$has("value"))
  getCache()$set("value", "one")
  expect_true(getCache()$has("value"))

})

test_that_cache("Given I have two caches with different names, When I work with the caches, Then values are stored separately", {

  initCache("another")

  getCache()$set("default", "one")
  getCache("another")$set("another", "one")

  expect_true(getCache()$has("default"))
  expect_false(getCache()$has("another"))

  expect_false(getCache("another")$has("default"))
  expect_true(getCache("another")$has("another"))
})

# TODO try to init cache already init'ed
# TODO try to get cache that hasn't been inited

# TODO init and get with different name!

