assert_storage_contract <- function(storage) {
  storage.set(storage, "string", "one")
  storage.set(storage, "number", 100)
  storage.set(storage, "logical", TRUE)

  expect_true(storage.has(storage, "string"))
  expect_equal(storage.get(storage, "string"), "one")
  expect_equal(storage.get(storage, "number"), 100)
  expect_equal(storage.get(storage, "logical"), TRUE)

  storage.unset(storage, "string")
  expect_false(storage.has(storage, "string"))
  expect_null(storage.get(storage, "string"))

  storage.clear(storage)
  expect_false(storage.has(storage, "number"))
  expect_false(storage.has(storage, "logical"))
  expect_null(storage.get(storage, "number"))
  expect_null(storage.get(storage, "logical"))
}
