library(magrittr)

# Hash function
hash <- function(value) digest::digest(value)

# Helper to create hash from function and args details.
hashFunctionCall <- function(name, formals, args) {

  # get default args
  args.default <- formals[
    unlist(
      lapply(formals,
             function(arg) {arg != ""}))]

  # get unset default args
  args.set <- c(
    args,
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args)}))])

  # get args hash
  name %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
}

##
#' @title
#' Memoises a given function
#' @description
#' Memoises a given function such that the result of the function is cached to improve
#' function performance
#' @param f Function to memoise.
#' @return
#' Memoised function
#' @examples
#' # create a simple memo function
#' memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # call first time
#' memo(10)               # prints "Getting some value" and returns 10
#'
#' # call second time
#' memo(10)               # immediately returns 10
#'
#' # can use 'force' parameter to ensure method is executed despite
#' # being value being previously cached.
#' memo(10, force=TRUE)   # prints "Getting some value" and returns 10
#' memo(10)               # immediately returns 10
#' memo(10, force=FALSE)  # immediately returns 10
#' @export
##
memo <- function (f) {

  # get cache
  f.cache <- f %>% hash() %>% cache()

  # grab the information about the function
  f.formals <- formals(f)

  # create the memo function
  f.memo <- function (force=FALSE) {

    # grab the call
    call <- match.call()

    # remove force from call if present
    if (!is.null(call[["force"]])) call[["force"]] <- NULL

    # generate hash from function name and arguments
    hash <- hashFunctionCall(call[[1]], formals(), as.list(call[-1]))

    # if force or cached
    if (!force && f.cache$has(hash)) {

      # returned cached value
      f.cache$get(hash)

    } else {

      # tweak call to call f and remove force
      call[[1]] <- quote(f)

      # evaluate call and cache results
      f.cache$set(hash, eval(call))
    }
  }

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(f.formals, formals(f.memo))

  # return the memo function
  f.memo
}

##
#' @title Is this a memo function
#' @description Checks whether the passed function is a memo function.
#' @param f Function, memo or otherwise
#' @return \code{TRUE} if memo function, \code{FALSE} otherwise
#' @examples
#' # create a simple memo function
#' memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # check whether function is a memo
#' is.memo(print) # expect FALSE
#' is.memo(memo)  # expect TRUE
#' @export
##
is.memo <- function(f) "f.memo" %>% exists(envir=environment(f))

##
#' @title Get memo function cache
#' @description Gets the cache associated with a memo function allowing further manipulation and control of the underlying values
#' being stored.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f Memo function
#' @return Cache storing values for memoed function.
#' @examples
#' # create a simple memo function
#' memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # make a couple of calls to memoed function to populate cache
#' memo(10)
#' memo(20)
#' memo(30)
#'
#' # get the memo's cache
#' cache <- memo.cache(memo)
#'
#' # clear the contents of the cache
#' cache$ls()     # expect to see cache entries from memo calls
#' cache$clear()
#' cache$ls()     # expect to see cache is now empty
#'
#' # calling memo function again will repopulate cache
#' memo(10)
#' @export
##
memo.cache <- function(f) {

  if (!is.memo(f)) stop("Invalid parameter - memo is nor a memoised function")

  "f.cache" %>% get(envir=environment(f))
}

##
#' @title Get memo function origional function
#' @description Gets the original function that was memoised.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f Memo function
#' @return Original unmemoised function.
#' @examples
#' # create a simple memo function
#' memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # get the origional function
#' orig <- memo.function(memo)
#'
#' # call the original function
#' orig(10)
#' @export
##
memo.function <- function(f) {

  if (!is.memo(f)) stop("Invalid parameter - memo is nor a memoised function")

  "f" %>% get(envir=environment(f))
}
