library(magrittr)

source("./R/helper.R")

##
#' @title
#' Memoises a given function
#' @description
#' Memoises a given function such that the result of the function is cached to improve
#' function performance
#' @param f function to memoise
#' @param allow.null indicates whether results are cached when a function returns NULL, default value FALSE
#' @return
#' Memoised function
#' @examples
#' # create a simple memo function
#' my.memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # call first time
#' my.memo(10)               # prints "Getting some value" and returns 10
#'
#' # call second time
#' my.memo(10)               # immediately returns 10
#'
#' # can use 'force' parameter to ensure method is executed despite
#' # being value being previously cached.
#' my.memo(10, force=TRUE)   # prints "Getting some value" and returns 10
#' my.memo(10)               # immediately returns 10
#' my.memo(10, force=FALSE)  # immediately returns 10
#' @export
##
memo <- function (f, allow.null=FALSE) {
  
  # TODO add option to prevent "force" and others being added to formals of memo function
  # TODO providing some sort of id, means the memo could use a shared cache
  # TODO provide a way to supply cache arguments, for example the algo to use, max size, storage strategy, etc

  # get cache
  f.cache <- cache()

  # TODO add 'dryRun' parameter 
  # TODO change name of 'force" to 'memo.force' to reduce chance of name clash
  
  # create the memo function
  f.memo <- function (force=FALSE) {

    # get the function call
    fc <- functionCall()
    
    # generate hash
    hash <- hash(fc)

    # if force or cached
    if (!force && f.cache$has(hash)) {

      # returned cached value
      f.cache$get(hash)

    } else {

      if ("force" %in% names(fc$args)) fc$args[["force"]] <- NULL
      
      # get the result
      result <- do.call(f, fc$args)

      if (!is.null(result) || allow.null) {
        
        # cache the result
        f.cache$set(hash, result)
      }
      
      result
    }
  }

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(formals(f), formals(f.memo))

  # return the memo function
  f.memo
}

##
#' @title Is this a memo function
#' @description Checks whether the passed function is a memo function.
#' @param f function, memo or otherwise
#' @return \code{TRUE} if memo function, \code{FALSE} otherwise
#' @examples
#' # create a simple memo function
#' my.memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # check whether function is a memo
#' is.memo(print) # expect FALSE
#' is.memo(my.memo)  # expect TRUE
#' @export
##
is.memo <- function(f) "f.memo" %>% exists(envir=environment(f))

##
#' @title Get memo function cache
#' @description Gets the cache associated with a memo function allowing further manipulation and control of the underlying values
#' being stored.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Cache storing values for memoed function.
#' @examples
#' # create a simple memo function
#' my.memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # make a couple of calls to memoed function to populate cache
#' my.memo(10)
#' my.memo(20)
#' my.memo(30)
#'
#' # get the memo's cache
#' cache <- memo.cache(my.memo)
#'
#' # clear the contents of the cache
#' cache$ls()     # expect to see cache entries from memo calls
#' cache$clear()
#' cache$ls()     # expect to see cache is now empty
#'
#' # calling memo function again will repopulate cache
#' my.memo(10)
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
#' @param f memo function
#' @return Original unmemoised function.
#' @examples
#' # create a simple memo function
#' my.memo <- memo(function (someValue) {
#'   print("Getting some value")
#'   someValue
#' })
#'
#' # get the origional function
#' orig <- memo.function(my.memo)
#'
#' # call the original function
#' orig(10)
#' @export
##
memo.function <- function(f) {

  if (!is.memo(f)) stop("Invalid parameter - memo is nor a memoised function")

  "f" %>% get(envir=environment(f))
}

## TODO add memo.force(memo, force=TRUE) - forces the execution of memo .. sets memo.force to value in environment(f) to temproarily override value
## TODO add memo.force<-(memo, force) - sets the memo.force value perminantly in the evironment(f)
## TODO add is.memo.force - gets the current value of memo.force from the environment
