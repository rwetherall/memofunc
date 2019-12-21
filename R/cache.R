library(digest)
library(uuid)

##
#' @title Simple cache used to store the values of memoised functions
#' @description A simple in memory cache used to store the value of memoised functions.
#' @return A cache which can be accessed using:
#' \itemize{
#'   \item cache()#set - set the value of a given key
#'   \item cache()#get - get the value of a given key
#'   \item cache()#unset - clear the value for a given key
#'   \item cache()#has - true if given key has value, false otherwise
#'   \item cache()clear - clears all values and keys
#'   \item cache()#ls - lists all the values and keys 
#'   }
#' @export
##
cache <- function () {

  # environment used as memory storage
  memoryStorage <- new.env(parent=emptyenv())

  # Set value for given key
  set <- function(key, value) {

    # store value in memory
    assign(key, value, envir=memoryStorage)

    # return value since assign return is invisible
    value
  }

  # get value with key
  get <- function(key) {

    if (exists(key, envir=memoryStorage)) base::get(key, envir=memoryStorage) else NULL
  }

  # unset value with key
  unset <- function(key) {

    # remove for memory
    if (exists(key, envir=memoryStorage)) rm(list=c(key), envir=memoryStorage)

  }

  # has value with key
  has <- function(key) exists(key, envir=memoryStorage)

  # clear all
  clear <- function() {

    # clear memory
    rm(list=base::ls(memoryStorage), envir=memoryStorage)

  }

  # list cache contents
  ls <- function() base::ls(memoryStorage) 

  # combine cache and storage features
  list(set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
