context("hash")

library(magrittr)
library(testthat)

##
# Helper to test whether two functions are the same
#
expect_list_equal <- function (given, expected) {
  given %>% length() %>% expect_equal(length(expected))
  mapply(identical, names(expected), names(given)) %>% all() %>% expect_true(label=paste(names(expected), " equals ", names(given)))
  mapply(identical, expected, given) %>% all() %>% expect_true(label=paste(expected, " equals ", given))
}

##
# Helper to test that we get the expected default arguments for a function
#
test_that_defaultArgs_expected <- function (fn.test, result.expected) {
  
  # get functions default arguments
  fn.test %>% formals() %>% defaultArgs() %>%
  
  # check we get the expected results
  expect_list_equal(result.expected)
  
}

test_that("
  Given a list,
  When I ask for the first n of a list
  Then I get the first n of a list", {
    
  values <- list(1,2,3,4,5)
  
  expect_list_equal(list(1,2,3), first.n(values, 3))
  expect_list_equal(list(1,2,3), values %<n>% 3)
})

test_that("
  Given some arguments, 
  When I ask which have not been named, 
  Then I get a list the missing names in the order they appear in the argument list", {
  
  test.fn <- function (a, b=10, c=10) NULL

  expect_list_equal(
    unused.formals(formals(test.fn), list(10, 10, 10)),
    list("a", "b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(a=10, 10, 10)),
    list("b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(10, 10, c=10)),
    list("a", "b"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(10, a=10, 10)),
    list("b", "c"))
  
  expect_list_equal(
    unused.formals(formals(test.fn), list(b=10, a=10, c=10)),
    list())
})

test_that("
  Given a set of arguments with or without names, 
  When I ask for their names, 
  Then I get the full list of argument names in the order they appear", {
  
  test.fn <- function (a, b=10, c=10, d, e) NULL
  
  expect_list_equal(
    all.names(formals(test.fn), list(10, 10, 10, 10, 10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(a=10, 10, c=10, 10, 10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(a=10, b=10, c=10, d=10, e=10)),
    list("a", "b", "c", "d", "e"))
  
  expect_list_equal(
    all.names(formals(test.fn), list(b=10, 10, d=10, 10, 10)),
    list("b", "a", "d", "c", "e"))
  
})

test_that("
  Given a function, 
  When I ask for the default values, 
  Then I get them", {
  
  # no default values
  test_that_defaultArgs_expected(
    function (value) NULL,
    alist()
  )
  
  # only default values
  test_that_defaultArgs_expected(
    function (def.value1 = "text", def.value2 = 10, def.value3 = list()) NULL,
    alist(def.value1 = "text", def.value2 = 10, def.value3 = list())
  )
  
  # mixed default and non-default values
  test_that_defaultArgs_expected(
    function (value1, def.value1 = 10, value2, value3, def.value2 = 20) NULL,
    alist(def.value1 = 10, def.value2 = 20)
  )
  
  # NULL, NA or empty default values
  test_that_defaultArgs_expected(
    function (def.value1 = NULL, def.value2 = "", def.value3 = NA) NULL,
    alist(def.value1 = NULL, def.value2 = "", def.value3 = NA)
  )
  
  # with elipse
  test_that_defaultArgs_expected(
    function (def.value1 = 10, ...) NULL,
    alist(def.value1 = 10)
  )
  
  # varible reference
  myValue <- "PEFW"
  test_that_defaultArgs_expected(
    function (def.value1 = myValue) NULL,
    alist(def.value1 = myValue)
  )
  
})

test_that("
  Given a list of default arguments, 
  When I inspect them against the provided arguments, 
  Then I will get the unset default values", {
  
  alist(y=20, z=20) %>% 
    unset.defaultArgs(alist(10, x=10, y=10)) %>%
    expect_list_equal(list(z=20)) 
})

test_that("
  Given a function and a call, 
  When I ask for the function call, 
  Then the arguments are fully named correctly", {

  test.fn <- function (a, b=10, c=10, d) NULL
  
  test.call1 <- call("test.fn", 20, b=30)
  test.call2 <- call("test.fn", 20, b=30, c=10)
  test.call3 <- call("test.fn", b=30, a=20)
  test.call4 <- call("test.fn", 20, 30)
  test.call5 <- call("test.fn", 20, 30, a=10, 40)
  
  with (functionCall(test.fn, test.call1), expect_list_equal(args, list(a=20, b=30)))
  with (functionCall(test.fn, test.call2), expect_list_equal(args, list(a=20, b=30, c=10)))
  with (functionCall(test.fn, test.call3), expect_list_equal(args, list(b=30, a=20)))
  with (functionCall(test.fn, test.call4), expect_list_equal(args, list(a=20, b=30)))
  with (functionCall(test.fn, test.call5), expect_list_equal(args, list(b=20, c=30, a=10, d=40)))
})

test_that("
  Given a function, 
  When I ask for the function call, 
  Then I get the arguments fully named 
  And the function 
  And the calling name", {
    
  test.fn <- function (a, b=10, c=10, d) functionCall()

  expect_equal(test.fn(20, 10)$f, test.fn)
  expect_equal(test.fn(20, 20)$name, quote(test.fn))
  
  with (test.fn(20, b=30), expect_list_equal(args, list(a=20, b=30)))
  with (test.fn(20, b=30, c=10), expect_list_equal(args, list(a=20, b=30, c=10)))
  with (test.fn(b=30, a=20), expect_list_equal(args, list(b=30, a=20)))
  with (test.fn(20, 30), expect_list_equal(args, list(a=20, b=30)))
  with (test.fn(20, 30, a=10, 40), expect_list_equal(args, list(b=20, c=30, a=10, d=40)))

  test.fn.elip <- function (a, b=10, ...) functionCall()
  
  with (test.fn.elip(20, b=30), expect_list_equal(args, list(a=20, b=30)))
  with (test.fn.elip(20, b=30, c=10), expect_list_equal(args, list(a=20, b=30, c=10)))
  with (test.fn.elip(b=30, a=20), expect_list_equal(args, list(b=30, a=20)))
  with (test.fn.elip(20, 30), expect_list_equal(args, list(a=20, b=30)))
  
  with (test.fn.elip(20, a=10, 30, 40), expect_list_equal(args, list(b=20, a=10, mf.na=30, mf.na=40)))
  with (test.fn.elip(10, 20, 30, 40), expect_list_equal(args, list(a=10, b=20, mf.na=30, mf.na=40)))
  with (test.fn.elip(10, 20, c=30, d=40), expect_list_equal(args, list(a=10, b=20, c=30, d=40)))
  with (test.fn.elip(10, 20, 30, d=40), expect_list_equal(args, list(a=10, b=20, mf.na=30, d=40)))
})

test_that("
  Given a function with mandatory and non-mandatory arguments, 
  When I hash the function call with the same argurment values in different order, 
  Then the result is always identical", {
  
  test.fn <- function (a, b=10, c=10) NULL
  
  test.call1 <- call("test.fn", 20, b=30)
  test.call2 <- call("test.fn", 20, b=30, c=10)
  test.call3 <- call("test.fn", b=30, a=20)
  test.call4 <- call("test.fn", 20, 30)
  test.call5 <- call("test.fn", 20, 30, 10)
  
  hash1 <- functionCall(test.fn, test.call1) %>% hash()
  hash2 <- functionCall(test.fn, test.call1) %>% hash()
  hash3 <- functionCall(test.fn, test.call2) %>% hash()
  hash4 <- functionCall(test.fn, test.call3) %>% hash()
  hash5 <- functionCall(test.fn, test.call4) %>% hash()
  hash6 <- functionCall(test.fn, test.call5) %>% hash()
  
  expect_equal(hash1, hash2)
  expect_equal(hash1, hash3)
  expect_equal(hash1, hash4)
  expect_equal(hash1, hash5)
  expect_equal(hash1, hash6)
  
})

all_this <- function(this, values) 
  if (length(values) <= 1) TRUE else
    values[2:length(values)] %>% sapply(this, y = values[[1]]) %>% all() && all_this(this, values[2:length(values)])

all_different <- function(values) all_this(`!=`, values)
all_same <- function (values) all_this(`==`, values)

test_that("
  Given a function with mandatory and non-mandatory arguments, 
  When I hash the function call with different argument values, 
  Then the results are all different", {
    
  test.fn <- function (a, b=10, c=10) NULL
    
  list(
    call("test.fn", 20, b=30),
    call("test.fn", 20, b=30, c=5),
    call("test.fn", b=30, a=10),
    call("test.fn", 20, 10),
    call("test.fn", 25, 30, 10)) %>%
    
  lapply(function (call) functionCall(test.fn, call) %>% hash()) %>%
  
  all_different() %>% expect_true()
})

test_that("
  Given a function with mandatory and non-mandatory arguments, 
  When I hash the function call with varibale argument values, 
  Then the results reflect the changes to the value", {
  
  test.fn <- function (a, b=10, c=10) NULL
  
  alpha <- 20
  hash1 <- functionCall(test.fn, call("test.fn", alpha)) %>% hash()
  
  alpha <-10
  hash2 <- functionCall(test.fn, call("test.fn", alpha)) %>% hash()
  
  alpha <- 20
  hash3 <- functionCall(test.fn, call("test.fn", alpha)) %>% hash()
  
  expect_false(hash1 == hash2)
  expect_true(hash1 == hash3)
})

test_that("Given a function with elipse,
When I hash the function call 
Then the hash is equivalent for additional arguments of the same names and order
And the has is different when the additional arguments are in a different order or have a different name
  ", {
   
    test.fn <- function (a, b=10, ...) NULL
    
    list(
      call("test.fn", 10, 20, 30, d=40, 50),
      call("test.fn", a=10, b=20, 30, d=40, 50),
      call("test.fn", b=20, a=10, 30, d=40, 50),
      call("test.fn", b=20, a=10, d=40, 30, 50)
    ) %>%
    lapply(function (call) functionCall(test.fn, call) %>% hash()) %>%  
    all_same() %>% expect_true();
    
    values <- list(
      call("test.fn", 10, 20, 30, d=40, 50), 
      call("test.fn", 10, 20, d=30, 40, 50),
      call("test.fn", 10, 20, f=30, 40, 50),
      call("test.fn", 10, 20, 50, 40, 30)
    ) %>%
    lapply(function (call) functionCall(test.fn, call) %>% hash())
    
    all_different(values) %>% expect_true(label = paste(values, " are all different"))
})

test_that("
  Given a function, 
  When I evaluate it using do.call, 
  Then I get the same hash from the function call as I would if I invoked the function normally", {
  
  test.fn <- function (a, b=10) functionCall() %>% hash()
  
  expect_equal(
    do.call(test.fn, list(10, b=20)),
    test.fn(10, 20)
  )
  
  expect_equal(
    do.call(test.fn, list(b=10, a=20)),
    test.fn(20, b=10)
  )
})

test_that("
  Given a function that takes a function as a parameter,
  When I hash the function call,
  Then anonumous functions that are identical in body to function varibles produce the same hash values", {

  test.add <- function (x) x+1
  test.subtract <- function (x) x-1

  test.fn <- function (f, x=10) f(x)
  value <- 10

  list(
    call("test.fn", test.add),
    call("test.fn", test.add, 10),
    call("test.fn", test.add, value),
    call("test.fn", function (x) x+1, 10)) %>%
  lapply(function (call) functionCall(test.fn, call) %>% hash()) %>%
  all_same() %>% expect_true()

  list(
    call("test.fn", test.add, 20),
    call("test.fn", test.add, 10),
    call("test.fn", test.subtract, 20),
    call("test.fn", function (x) x+1, 30)) %>%
  lapply(function (call) functionCall(test.fn, call) %>% hash()) %>%
  all_different() %>% expect_true()

})

# TODO Note: anonymouse functions can't be hashed atm!
