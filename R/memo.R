library(magrittr)

##
#' @title Memo
#' @description
#' Creates a memoized function, based on the provided named or anonymous function.  Calls to the memoized function will 
#' be retrieved from a cache, unless it is the first time it is called.
#' 
#' Passing \code{memo.force = TRUE} to the memo function call will by-pass any previously cached values and execute the underlying
#' function, storing the newly retrieved values for subsequent calls.  \code{memo.force = FALSE} by default.
#' 
#' Passing \code{memo.dryrun = TRUE} to the memo function call will prevent the underlying function from executing and return TRUE
#' if call isn't caches and \code{FALSE} if it is.  These values are not cached as responses for the function.
#' 
#' Note that results are cached based on the argument values passed to the function.  The order is not important since all
#' names are resolved.  So \code{fun(a=1, b=2)} will return the same cached value as \code{fun(b=2, a=1)}, for example.
#' 
#' Functions as arguments are supported, but only the body is compared.  So a named function parameter and an anonymouse function
#' parameter with the same body, will be evaluated as identical and return the same cached value.
#' 
#' \code{...} is supported, but note that unless named then the order of the values is significant and will produce different cache values
#' unless identical.
#' 
#' By default \code{NULL} values are not cached.  Setting \code{allow.null=TRUE} when creating the memo will, however, ensure that NULL values
#' are cached.
#' @param f function to memoise
#' @param allow.null if \code{TRUE} then the memoed function will cache \code{NULL} results, otherwise it won't.  \code{FALSE} by default.
#' @return the memoed function
#' @example R/examples/memo/example.memo.R
#' @export
##
memo <- function (f, allow.null=FALSE) {
  
  # you can't memo a memo
  stopifnot(!is.memo(f))
  
  # TODO add option to prevent "force" and others being added to formals of memo function
  # TODO providing some sort of id, means the memo could use a shared cache (or use the hash of the fn to lookup a cache)
  # TODO provide a way to supply cache arguments, for example the algo to use, max size, storage strategy, etc

  # get cache
  f.cache <- storage.init("memory")
  
  # create the memo function
  f.memo <- function (memo.force=FALSE, memo.dryrun=FALSE) {

    # get the function call
    fc <- functionCall()
    
    # remove memo args we have added to the function
    fc$args %<>% removeby.name("memo.force") %>% removeby.name("memo.dryrun")
    
    # generate hash
    hash <- hash(fc)

    # if force or cached
    if (!memo.force && storage.has(f.cache, hash)) {

      # false if dry run otherwise cached value
      if (memo.dryrun) FALSE else storage.get(f.cache, hash)
      
    } else {

      # TRUE if dry run otherwise call the function
      result <- if (memo.dryrun) TRUE else do.call(f, fc$args)

      # handle null results
      if (!is.null(result) || allow.null) {
        
        # cache the result
        if (!memo.dryrun) storage.set(f.cache, hash, result)
      }
      
      result
    }
  }

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(formals(f), formals(f.memo))
  
  # set the class of the memo function
  class(f.memo) <- c("memo", class(f.memo))

  # return the memo function
  f.memo
}

##
#' @title Is Memo
#' @description Checks whether the passed function is a memo function.
#' @param f function, memo or otherwise
#' @return \code{TRUE} if memo function, \code{FALSE} otherwise
#' @export
##
is.memo <- function(f) inherits(f, "memo")

##
#' @title Memo Cache
#' @description Gets the cache associated with a memo function allowing further manipulation and control of the underlying values
#' being stored.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Cache storing values for memoed function.
#' @export
##
memo.cache <- function(f) {

  stopifnot(is.memo(f))

  "f.cache" %>% get(envir=environment(f))
}

##
#' @title Memo Function
#' @description Gets the original function that was memoized.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Original unmemoized function.
#' @export
##
memo.function <- function(f) {

  stopifnot(is.memo(f))

  "f" %>% get(envir=environment(f))
}