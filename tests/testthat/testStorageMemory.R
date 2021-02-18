context("storage")

library(uuid)
library(testthat)

test_that("
  Given the default parameter values, 
  When I initialise new memory storage, 
  Then a empty memory storage is provided", {

  # create memory storage
  storage <- storage.init()
  
  # check the details 
  expect_equal(class(storage), "storage")
  expect_equal(storage$type, "memory")
  expect_false(is.null(storage$env))
  expect_equal(class(storage$env), "environment")

})

test_that("
  Given initiated memory storage, 
  When a value is set, 
  Then it can be retrieved", {

  # create memory storage
  storage <- storage.init()

  expect_equal((storage.set(storage, "string", "one")) %>% storage.get("string"), "one")
  expect_equal((storage.set(storage, "number", 100)) %>% storage.get("number"), 100)
  expect_equal((storage.set(storage, "logical", TRUE)) %>% storage.get("logical"), TRUE)

  myTestFn <- function() print("Hello")
  expect_equal((storage.set(storage, "function", myTestFn)) %>% storage.get("function"), myTestFn)
})

test_that("
  Given initiated memory storage, 
  When retieving a value that hasn't been set, 
  Then NULL is retrieved", {

  expect_null(storage.get(storage.init(), "value"))
})

test_that("
  Given initiated memory storage, 
  When a value is unset, 
  Then it is no longer in the cache", {

  # create memory storage
  storage <- storage.init()

  expect_equal(storage.set(storage, "value", "one") %>% storage.get("value"), "one")
  storage.unset(storage, "value")
  expect_null(storage.get(storage, "value"))

  # shouldn't issue warning
  storage.unset(storage, "value")
})

test_that("
  Given initiated memory storage, 
  When the storage is cleared, 
  Then all stored values are cleared", {

  # create memory storage
  storage <- storage.init()

  storage.set(storage, "value","one")
  storage.set(storage, "valuetoo", "two")
  expect_equal(storage.get(storage, "value"), "one")
  expect_equal(storage.get(storage, "valuetoo"), "two")

  storage.clear(storage)
  expect_null(storage.get(storage, "value"))
  expect_null(storage.get(storage, "valuetoo"))
})

test_that("
  Given initiated memory storage, 
  When the a value is reset, 
  Then the new value is stored", {

  # create memory storage
  storage <- storage.init()

  storage.set(storage, "value", "one")
  expect_equal(storage.get(storage, "value"), "one")
  storage.set(storage, "value", "two")
  expect_equal(storage.get(storage, "value"), "two")

})

test_that("
  Given initiated memory storage, 
  When I ask, 
  Then I can determine whether the storage contains a key value or not", {

  # create memory storage
  storage <- storage.init()

  expect_false(storage.has(storage, "value"))
  storage.set(storage, "value", "one")
  expect_true(storage.has(storage, "value"))

})

test_that("
  Given two initiated memory storage, 
  When I work with each storage, 
  Then values are stored separately", {

  # create memory storage
  storage <- storage.init()
  storage2 <- storage.init()

  storage.set(storage, "default", "one")
  storage.set(storage2, "another", "one")

  expect_true(storage.has(storage, "default"))
  expect_false(storage.has(storage, "another"))

  expect_false(storage.has(storage2, "default"))
  expect_true(storage.has(storage2, "another"))
})

test_that("
  Given initiated memory storage, 
  When I store a key with a NULL value, 
  Then I can retrive and check for existance successfully", {
  
  storage <- storage.init()
  
  storage.set(storage, "nullvalue", NULL);
  
  expect_true(storage.has(storage, "nullvalue"))
  expect_null(storage.get(storage, "nullvalue"))
  
})

# TODO check valid algo

# TODO check invalid algo

