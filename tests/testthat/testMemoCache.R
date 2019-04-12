context("memoCache")

library(testthat)

test_that("Given a valid cache type, When I try to create the cache, Then the correct type of cache is created" , {

  cache <- cache(type="memory")
  expect_equal(cache$type, "memory")
})

# TODO check valid algo

# TODO check invalid algo

test_that("Given I have created a valid custom cache type, When I try to create the cache, Then it is created and can be used", {

  addCacheType("myCacheType", function (name) {
    set <- function(key, value) "set"
    get <- function(key) "get"
    unset <- function(key) "unset"
    has <- function(key) "has"
    clear <- function() "clear"
    list (set = set, get = get, unset = unset, has = has, clear = clear)
  })

  cache <- cache(type="myCacheType")
  expect_equal(cache$set("key", "value"), "set")
  expect_equal(cache$get("key"), "get")
  expect_equal(cache$unset("key"), "unset")
  expect_equal(cache$has("key"), "has")
  expect_equal(cache$clear(), "clear")
})

#TODO check invalid cache types

# TODO check force parameter
