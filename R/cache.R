library(digest)

##
#' @title
#' Get a cache
#' @description
#' Gets a cache of the type specified taking into account any context provided
#' @param algo hasing algorithm which defaults to sha1
#' @return
#' A cache that can be used to store values
#' @export
##
cache <- function (algo="sha1") {

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
  set <- function(key, value) assign(key, value, envir=memoryStorage)

  # get value with key
  get <- function(key) if (exists(key, envir=memoryStorage)) base::get(key, envir=memoryStorage) else NULL

  # unset value with key
  unset <- function(key) if (exists(key, envir=memoryStorage)) rm(list=c(key), envir=memoryStorage)

  # has value with key
  has <- function(key) exists(key, envir=memoryStorage)

  # clear all
  clear <- function() rm(list=base::ls(memoryStorage), envir=memoryStorage)

  # list cache contents
  ls <- function() base::ls(memoryStorage)

  # combine cache and storage features
  list(algo = algo, hash = hash, hashFunctionCall = hashFunctionCall, set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
