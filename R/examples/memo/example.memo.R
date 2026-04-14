library(magrittr)

# a simple example function
simple.function <- function (value) {
  print("Executing!")
  value
}

# call memo function to memoise a function
simple.function.memo <- memo(simple.function)

# id defaults to the function name when available
attr(simple.function.memo, "memo.id")

# or like this
simple.function %<>% memo()

# or use an anon function
simple.function2 <- (function (value) value) %>% memo()

# use an explicit id for anonymous or generated functions
anonymous.memo <- memo(function (value) value, id = "example/anon")

# use an explicit id to keep cache stable across renames or wrappers
renamed.function <- simple.function
renamed.memo <- memo(renamed.function, id = "simple.function")

# the first time we call the memo the function will execute
simple.function(10)

# if we call the memo again with the same parameter values then
# the cached value will be returned
simple.function(10)

# calling the memo with a different set of parameter values will
# cause the function to execute
simple.function(20)

# consider a slow function which is memoised, note that we have used the allow.null argument
# so that NULL is cached when returned from a function, the default is FALSE
slow.function <- (function (value) Sys.sleep(value)) %>% memo(allow.null = TRUE)

# the first time we call the slow function it takes some time
system.time(slow.function(3))

# subsequent calls make use of the cache and are much faster
system.time(slow.function(3))
