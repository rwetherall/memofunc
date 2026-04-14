context("storage provider selection")

library(testthat)

test_that(
  "
  Given an object store with a provider name,
  When I initialize storage,
  Then the provider is resolved and used",
  {
    base.dir <- file.path(tempdir(), "memofunc-provider-object")
    storage <- storage.init(storage.object.class, provider = "file", base.dir = base.dir)

    expect_true(inherits(storage, "object"))
    storage.set(storage, "value", "one")
    expect_true(storage.has(storage, "value"))
    expect_equal(storage.get(storage, "value"), "one")
  }
)

test_that(
  "
  Given a default provider option,
  When I initialize storage with defaults,
  Then the provider is used",
  {
    base.dir <- file.path(tempdir(), "memofunc-provider-default")
    old.options <- options(memofunc.storage.provider = "file")
    on.exit(options(old.options), add = TRUE)

    storage <- storage.init(base.dir = base.dir)

    expect_true(inherits(storage, "file"))
    expect_equal(storage$base.dir, base.dir)
    storage.set(storage, "value", "one")
    expect_true(storage.has(storage, "value"))
    expect_equal(storage.get(storage, "value"), "one")
  }
)

test_that(
  "
  Given provider options with config,
  When I initialize storage,
  Then provider config is applied",
  {
    base.dir <- file.path(tempdir(), "memofunc-provider-config")
    old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))
    on.exit(options(old.options), add = TRUE)

    storage <- storage.init()

    expect_true(inherits(storage, "file"))
    expect_equal(storage$base.dir, base.dir)
  }
)
