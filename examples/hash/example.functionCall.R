# my example function
my.function <- function (x, y) x+y

# create a new function call
my.functionCall <- functionCall(my.function, call("my.function", 10, 10))

# the function and arguments are now available
my.functionCall$f
my.functionCall$args

# using the default argument values to get the function call of the containing function
my.function2 <- function (x, y) functionCall()
my.functionCall2 <- my.function2(10, 10)

# the function and arguments are now available
my.functionCall2$f
my.functionCall2$args