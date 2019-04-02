context("memoryMemoCache")

library(testthat)

#
# Clears all caches based on naming convention
#
clear_all_caches <- function () {
  remove(list = ls(pattern = "^memocache*", envir=sys.frame(0)), envir = sys.frame(0))
  cacheRegistry <<-list()
}

#
# Custom test method to ensure caches are cleared before
# and after tests are executed.
#
# Note:  this will clear any existing caches you have have
#        populated during normal use.
#
test_that_cache <- function(desc, code) {

  clear_all_caches()
  test_that(desc, code)
  clear_all_caches()
}

test_that_cache("Given the default cache settings, When I access the cache for the first time, Then a memory cache named default is created", {

  # ensure we are starting from fresh
  expect_false(exists("memocachedefault", envir=sys.frame(0)))

  # assess the default memory cache
  result <- initCache()

  # check that the cache storage has been created
  expect_true(exists("memocachedefault", envir=sys.frame(0)))

  # check that the expected 'methods' are avilable on the cache
  expect_true(is.function(result$set))
  expect_true(is.function(result$get))
  expect_true(is.function(result$unset))
  expect_true(is.function(result$clear))
})


test_that_cache("Given the default cache settings, When a value is set, Then it can be retrieved", {

  initCache()

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

  initCache()

  expect_null(getCache()$get("value"))
})

test_that_cache("Given the default cache settings, When a value is unset, Then it is no longer in the cache", {

  initCache()

  expect_equal(getCache()$set("value", "one"), "one")
  expect_equal(getCache()$get("value"), "one")
  getCache()$unset("value")
  expect_null(getCache()$get("value"))
})

test_that_cache("Given the default cache settings, when the cache is clear, Then it no longer exists", {

  initCache()

  getCache()$set("value","one")
  getCache()$set("valuetoo", "two")
  expect_equal(getCache()$get("value"), "one")
  expect_equal(getCache()$get("valuetoo"), "two")

  getCache()$clear()
  expect_null(getCache()$get("value"))
  expect_null(getCache()$get("valuetoo"))
})

test_that_cache("Given the default cache settings, When the a cache value is reset, Then the new value is taken into the cache", {

  initCache()

  getCache()$set("value", "one")
  expect_equal(getCache()$get("value"), "one")
  getCache()$set("value", "two")
  expect_equal(getCache()$get("value"), "two")

})

test_that_cache("Given the default cache settings, When I ask, Then I can determine whether the cache contains a key value or not", {

  initCache()

  expect_false(getCache()$has("value"))
  getCache()$set("value", "one")
  expect_true(getCache()$has("value"))

})

test_that_cache("Given I have two caches with different names, When I work with the caches, Then values are stored separately", {

  initCache()
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

