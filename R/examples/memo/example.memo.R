library(magrittr)

# a simple example function
simple.function <- function (value) {
  print("Executing!")
  value
}

# call memo function to memoise a function
simple.function.memo <- memo(simple.function)

# or like this
simple.function %<>% memo()

# or use an anon function
simple.function2 <- (function (value) value) %>% memo()

# the first time we call the memo the function will execute
simple.function(10)

# if we call the memo again with the same parameter values then
# the cached value will be returned
simple.function(10)

# calling the memo with a different set of parameter values will
# cause the function to execute
simple.function(20)
