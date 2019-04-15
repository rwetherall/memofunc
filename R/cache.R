library(digest)

##
#' @title
#' Create a cache.
#' @description
#' TODO
#' @param storage Persistant cache storage. If none provided cache is transient and held in memory.  NULL by default.
#' @return A cache that stores values for later retieval.
#' @export
##
cache <- function (storage = NULL) {

  # environment used as memory storage
  memoryStorage <- new.env(parent=emptyenv())

  # Set value for given key
  set <- function(key, value) {

    if (!is.null(storage)) {
      # TODO store value
    }

    # store value in memory
    assign(key, value, envir=memoryStorage)
  }

  # get value with key
  get <- function(key) {

    # check memory first
    if (exists(key, envir=memoryStorage)) {

      # get value from memory
      base::get(key, envir=memoryStorage)

    } else {

      if (!is.null(storage)) {

        # TODO try to get the value from storage

        # TODO store in memory
      }
    }
  }

  # unset value with key
  unset <- function(key) {

    # remove for memory
    if (exists(key, envir=memoryStorage)) rm(list=c(key), envir=memoryStorage)

    if (!is.null(storage)) {

      # TODO remove from storage

    }
  }

  # has value with key
  has <- function(key) !is.null(get(key))

  # clear all
  clear <- function() {

    # clear memory
    rm(list=base::ls(memoryStorage), envir=memoryStorage)

    if (!is.null(storage)) {

      # TODO clear storage

    }
  }

  # list cache contents
  ls <- function(memoryOnly = TRUE) {

    if (memoryOnly) base::ls(memoryStorage) # else TODO list from storage
  }

  # combine cache and storage features
  list(set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
