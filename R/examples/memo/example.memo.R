library(magrittr)

# a simple example function
simple.function <- function (value) value

# call memo function to memoise a function
simple.function.memo <- memo(simple.function)

# or like this
simple.function %<>% memo()

# or use an anon function
simple.function2 <- (function (value) value) %>% memo()
