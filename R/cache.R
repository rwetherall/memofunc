library(digest)

#' # Envirnoment used to store the different cache types
#' cache.env <- new.env(parent=emptyenv())
#'
#' ##
#' # Create cache type key from cache type
#' #
#' cacheTypeKey <- function (type) paste("type", type, sep=".")
#'
#' ##
#' #' @title
#' #' Adds a cache type to those available
#' #' @description
#' #' Adds a cache type to the registry of those available for initialisation.
#' #'
#' #' A cache type is defined as a function that retuns a list of functions bound to a specific cache name as follows:
#' #' * set(key, value) - sets a value for a key, overwrites any existing values at that key
#' #' * get(key) - gets the value of a key, NULL otherwise
#' #' * unset(key) - unsets value of a key
#' #' * has(key) - true if the key has a value, false otherwise
#' #' * clear() - clears all values
#' #' @param type name of the cache type
#' #' @param f function that returns a list of functions bound to a cache name
#' #' @return
#' #' list of functions that can be used to manipulate the named cache
#' #' @examples
#' #'
#' #' @export
#' ##
#' addCacheType <- function(type, f) {
#'
#'   # register the cache type
#'   assign(cacheTypeKey(type), f, envir=cache.env)
#' }

##
#' @title
#' Get a cache
#' @description
#' Gets a cache of the type specified taking into account any context provided
#' @param storage cache storage, defaults to memory storage if not specified
#' @param algo hasing algorithm which defaults to sha1
#' @return
#' A cache that can be used to store values
#' @export
##
cache <- function (storage = NULL, algo="sha1") {

  # check passed storage
  if (is.null(storage)) {
    # Use memory storage if none specified
    storage <- memoryStorage()
  } else {
    # TODo verify that the passed storage is valid
  }

  #
  # Hash function
  #
  hash <- function(value) digest::digest(value, algo=algo)

  #
  # Helper to create hash from function and args details.
  #
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

  # verify the cache type
 #if (!exists(cacheTypeKey(type), envir=cache.env)) stop(paste("Undefined memo cache type:", type))

  # combine cache and storage features
  c(
    storage,
    list(algo = algo, hash = hash, hashFunctionCall = hashFunctionCall))
}
