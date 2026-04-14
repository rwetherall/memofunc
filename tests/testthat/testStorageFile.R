context("storage file")

library(testthat)

test_that(
  "
  Given file storage,
  When I set and get values,
  Then values are stored on disk",
  {
    base.dir <- file.path(tempdir(), "memofunc-file-storage")
    storage <- storage.init(storage.file.class, base.dir = base.dir)

    storage.set(storage, "value", "one")

    expect_true(storage.has(storage, "value"))
    expect_equal(storage.get(storage, "value"), "one")
    expect_true(file.exists(storage.file.key_path("value", base.dir)))
  }
)

test_that(
  "
  Given file storage,
  When I unset a value,
  Then it is removed from disk",
  {
    base.dir <- file.path(tempdir(), "memofunc-file-storage-unset")
    storage <- storage.init(storage.file.class, base.dir = base.dir)

    storage.set(storage, "value", "one")
    storage.unset(storage, "value")

    expect_false(storage.has(storage, "value"))
    expect_null(storage.get(storage, "value"))
    expect_false(file.exists(storage.file.key_path("value", base.dir)))
  }
)

test_that(
  "
  Given file storage,
  When I clear the store,
  Then all values are removed",
  {
    base.dir <- file.path(tempdir(), "memofunc-file-storage-clear")
    storage <- storage.init(storage.file.class, base.dir = base.dir)

    storage.set(storage, "value", "one")
    storage.set(storage, "valuetoo", "two")
    expect_true(storage.has(storage, "value"))
    expect_true(storage.has(storage, "valuetoo"))

    storage.clear(storage)

    expect_false(storage.has(storage, "value"))
    expect_false(storage.has(storage, "valuetoo"))
  }
)
