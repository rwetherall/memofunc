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
#' @export
##
memo <- function (f, allow.null=FALSE) {
  
  # you can't memo a memo
  stopifnot(!is.memo(f))
  
  # TODO add option to prevent "force" and others being added to formals of memo function
  # TODO providing some sort of id, means the memo could use a shared cache (or use the hash of the fn to lookup a cache)
  # TODO provide a way to supply cache arguments, for example the algo to use, max size, storage strategy, etc

  # get cache
  f.cache <- cache()
  
  # create the memo function
  f.memo <- function (memo.force=FALSE, memo.dryrun=FALSE) {

    # get the function call
    fc <- functionCall()
    
    # remove memo args we have added to the function
    fc$args %<>% removeby.name("memo.force") %>% removeby.name("memo.dryrun")
    
    # generate hash
    hash <- hash(fc)

    # if force or cached
    if (!memo.force && f.cache$has(hash)) {

      # false if dry run otherwise cached value
      if (memo.dryrun) FALSE else f.cache$get(hash)
      
    } else {

      # TRUE if dry run otherwise call the function
      result <- if (memo.dryrun) TRUE else do.call(f, fc$args)

      # handle null results
      if (!is.null(result) || allow.null) {
        
        # cache the result
        if (!memo.dryrun) f.cache$set(hash, result)
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
#' @title Is this a memo function
#' @description Checks whether the passed function is a memo function.
#' @param f function, memo or otherwise
#' @return \code{TRUE} if memo function, \code{FALSE} otherwise
#' @export
##
is.memo <- function(f) inherits(f, "memo")

##
#' @title Get memo function cache
#' @description Gets the cache associated with a memo function allowing further manipulation and control of the underlying values
#' being stored.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Cache storing values for memoed function.
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
#' @export
##
memo.function <- function(f) {

  if (!is.memo(f)) stop("Invalid parameter - memo is nor a memoised function")

  "f" %>% get(envir=environment(f))
}