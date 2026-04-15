library(magrittr)

base.dir <- file.path(tempdir(), "memofunc-example-invalidate")
unlink(base.dir, recursive = TRUE, force = TRUE)

old.options <- options(memofunc.storage.provider = list(name = "file", base.dir = base.dir))

counter <- new.env(parent = emptyenv())
counter$executed <- 0

expensive <- function (value) {
  counter$executed <- counter$executed + 1
  value * 10
}

memo.expensive <- memo(expensive, id = "invalidate-example")

# warm cache for two keys
memo.expensive(1)
memo.expensive(2)

# invalidate a single entry by arguments
invalidate(memo.expensive, list(value = 1))

counter$executed <- 0
memo.expensive(1)
memo.expensive(2)
counter$executed

# invalidate all entries for this memo only
invalidate(memo.expensive)

counter$executed <- 0
memo.expensive(1)
memo.expensive(2)
counter$executed

options(old.options)
unlink(base.dir, recursive = TRUE, force = TRUE)
