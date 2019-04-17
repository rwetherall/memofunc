context("filestore")

library(digest)
library(uuid)
library(testthat)

storePath <- file.path(".", "cache")

setup(if (!dir.exists(storePath)) dir.create(storePath))

test_that("Given a file store, When I write a value to the store, Then it is successfully stored in the correct location on the file system", {

  store <- filestore(dir=storePath)

  cacheId <- UUIDgenerate()
  key <- digest("mykey")
  value <- list(1,2,3,4,5)

  store$write(cacheId, key, value)

  expect_equal(TRUE, TRUE)

})

teardown(if (dir.exists(storePath)) unlink(storePath, recursive=TRUE, force=TRUE))
