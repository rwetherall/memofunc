my.function <- function (x, y) x+y

# a list of values to hash
values <- list(
  "Hello world!",
  101,
  3.142,
  TRUE,
  my.function,
  (function (x, y) x+y),
  functionCall(my.function, call("my.function", 10, 10)),
  list(a=1, b=2, c="hello")
)

# hash the values in the list
hashes <- lapply(values, hash)
hashes

# Note that functions with the same body will have the same hash
hashes[[5]] == hashes[[6]]