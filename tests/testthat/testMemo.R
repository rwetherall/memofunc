
context("memo")

library(magrittr)
library(testthat)

test.env <- NULL
key.executed <- "executed"

test_fn <- function (fn) {
  function (...) {
    assign(key.executed, TRUE, envir=test.env)
    fn(...)
  }
}

test_that_eval <- function(execute) {
  
  # create new test env
  test.env <<- test_env()
  
  # TODO record time taken
  
  list(
    value = eval(execute),
    executed = mget(key.executed, envir=test.env, inherits=FALSE, ifnotfound=FALSE)[[1]])
}

# Note: use do.call(fn, list(10, force=TRUE)) 
#
# return-value <- function (value) {value}
# 
# test_fn-calls <- list (
#   call("return-value", 10),
#   call("return-value", 10, force=TRUE)
# )

test_that("Given a simple function, When I memoise the function, Then the results are cached", {
  
  test1 <- function(value){value}

  test1.memo <- memo(test_fn(test1))
 
  with(test_that_eval({test1.memo(30)}), {
    expect_equal(value, 30)
    expect_true(executed)
  })
 
  with (test_that_eval({test1.memo(30)}), {
    expect_equal(value, 30)
    expect_false(executed)
  })
  
  with(test_that_eval({test1.memo(300)}), {
    expect_equal(value, 300)
    expect_true(executed)
  })
  
  with (test_that_eval({test1.memo(300)}), {
    expect_equal(value, 300)
    expect_false(executed)
  })

  # a call with force false doesn't execute
 #test_that_memo_executed(test1.memo(10, force=FALSE), 10, FALSE)
  
  # force call executes method
#  test_that_memo_executed(test1.memo(10, force=TRUE), 10, TRUE)
  
 # test_that_memo_cached(test1.memo, 20, 20, TRUE)

})

# test_that("Given a simple function, When I memoise the function in different environments, Then the caches share the same id", {
# 
#   evalq({
#     memo1 <- memo(test1)
#   },
#   envir=new.env())
# 
#   evalq({
#     expect_error(test)
#   },
#   envir=new.env())
# 
# })

test_that("Given a simple function, When I ask, Then I am told whether it is a memoised fn or not ", {
  
  # simple function
  test1 <- 
    function(value) {
      assign("executed", TRUE, envir=test.env)
      value
    }

  memo(test1) %>% is.memo() %>% expect_true()
  test1 %>% is.memo() %>% expect_false()
})

# TODO Given a memo'ed function make sure it can still access the hierarchy of environments correctly ..
#      for example global variables .. what if it's a closure?

