library(digest)

##
#' @title
#' Create a cache.
#' @description
#' TODO
#' @param algo Hasing algorithm. Defaults to \code{sha1}.
#' @param storage Persistant cache storage. If none provided cache is transient and held in memory.  NULL by default.
#' @return A cache that stores values for later retieval.
#' @export
##
cache <- function (algo="sha1", storage = NULL) {

  # environment used as memory storage
  memoryStorage <- new.env(parent=emptyenv())

  # Hash function
  hash <- function(value) digest::digest(value, algo=algo)

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
    hash(c(name, unlist(args.set, use.names = FALSE)))
  }

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
  list(algo = algo, hash = hash, hashFunctionCall = hashFunctionCall, set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
