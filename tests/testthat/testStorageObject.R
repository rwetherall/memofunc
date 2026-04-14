context("storage object")

library(testthat)

make.provider <- function() {
  env <- new.env(parent = emptyenv())
  list(
    put = function(key, value) assign(key, value, envir = env),
    get = function(key) if (exists(key, envir = env)) get(key, envir = env) else NULL,
    exists = function(key) exists(key, envir = env),
    delete = function(key) {
      if (exists(key, envir = env)) rm(list = key, envir = env)
      invisible(NULL)
    },
    clear = function() {
      rm(list = ls(env), envir = env)
      invisible(NULL)
    }
  )
}

test_that(
  "
  Given a provider,
  When I initialize object storage,
  Then it stores the provider and uses the correct class",
  {
    provider <- make.provider()
    storage <- storage.init(storage.object.class, provider = provider)

    expect_equal(class(storage), c("storage", "object"))
    expect_true(is.list(storage$provider))
  }
)

test_that(
  "
  Given object storage,
  When I set and get values,
  Then values are stored and retrieved",
  {
    storage <- storage.init(storage.object.class, provider = make.provider())

    storage.set(storage, "string", "one")
    storage.set(storage, "number", 100)
    storage.set(storage, "logical", TRUE)

    expect_equal(storage.get(storage, "string"), "one")
    expect_equal(storage.get(storage, "number"), 100)
    expect_equal(storage.get(storage, "logical"), TRUE)
  }
)

test_that(
  "
  Given object storage,
  When a value is unset,
  Then it is removed from the store",
  {
    storage <- storage.init(storage.object.class, provider = make.provider())

    storage.set(storage, "value", "one")
    expect_true(storage.has(storage, "value"))

    storage.unset(storage, "value")
    expect_false(storage.has(storage, "value"))
    expect_null(storage.get(storage, "value"))
  }
)

test_that(
  "
  Given object storage,
  When the storage is cleared,
  Then all values are removed",
  {
    storage <- storage.init(storage.object.class, provider = make.provider())

    storage.set(storage, "value", "one")
    storage.set(storage, "valuetoo", "two")

    storage.clear(storage)

    expect_false(storage.has(storage, "value"))
    expect_false(storage.has(storage, "valuetoo"))
    expect_null(storage.get(storage, "value"))
    expect_null(storage.get(storage, "valuetoo"))
  }
)

test_that(
  "
  Given no provider,
  When I initialize object storage,
  Then an error is raised",
  {
    expect_error(storage.init(storage.object.class))
  }
)
