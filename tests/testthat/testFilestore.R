context("filestore")

library(magrittr)
library(digest)
library(uuid)
library(testthat)

path.default <- file.path(".", "filestoretest")

store.create <- function (storePath=NULL, cacheId=NULL, data=NULL) {

  storePath.resolved <- if (is.null(storePath)) path.default else storePath

  # create store path
  if (!dir.exists(storePath.resolved)) dir.create(storePath.resolved)

  # create store
  store <- filestore(dir=storePath.resolved)

  # initialise store with provided data
  if (!is.null(data))
    lapply(names(data),
      function (key) {
        if (is.null(cacheId)) UUIDgenerate() else cacheId %>%
          store$write(key, data[[key]])
      })

  store
}

store.rm <- function (storePath=NULL) {

  storePath.resolved <- if (is.null(storePath)) path.default else storePath

  if (dir.exists(storePath.resolved)) unlink(storePath.resolved, recursive=TRUE, force=TRUE)
}

test_that(
  "Given a file store, When I write a value to the store, Then it is successfully stored in the correct location on the file system", {

  store.create(cacheId="myCache", data=list(one=1, two=2, three=3))

  dir.exists(path.default) %>% expect_true()
  file.path(path.default, "myCache") %>% dir.exists() %>% expect_true()
  file.path(path.default, "myCache", "one") %>% file.exists() %>% expect_true()
  file.path(path.default, "myCache", "two") %>% file.exists() %>% expect_true()
  file.path(path.default, "myCache", "three") %>% file.exists() %>% expect_true()

  store.rm()
})

test_that("Given a file store, When I try to call methods without provding a cacheId, Then I get an error", {
  expect_error(store.create()$write(NULL, "key", "value"))
  expect_error(store.create()$read(NULL, "key"))
  store.rm()
})

test_that(
  "Given a file store, When I try retireve a value for a non-existatant cache, Then I get NULL", {

  store.create()$read("madeup", "something") %>% expect_null()
  store.rm()
})

test_that("Given a file store, When I try to retieve a value for a non-existant key, Then I get NULL", {

  # create a store with some data
  store <- store.create(cacheId="myCache", data=list(one=1, two=2, three=3))

  # try to read an invalid key
  store$read("myCache", "something") %>% expect_null()
  store.rm()

})

test_that(
  "Given a file store, When I try to retieve a value for a valid key, Then I get the expected value", {

  # create a store with some data
  store <- store.create(cacheId="myCache", data=list(one=1, two=2, three=3))

  expect_equal(store$read("myCache", "one"), 1)
  expect_equal(store$read("myCache", "two"), 2)
  expect_equal(store$read("myCache", "three"), 3)

  store.rm()

})
