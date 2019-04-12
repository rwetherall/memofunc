library(digest)

cache.env <- new.env(parent=emptyenv())
cacheTypeKey <- function (type) paste("type", type, sep=".")

##
#' @title
#' Adds a cache type to those available
#' @description
#' Adds a cache type to the registry of those available for initialisation.
#'
#' A cache type is defined as a function that retuns a list of functions bound to a specific cache name as follows:
#' * set(key, value) - sets a value for a key, overwrites any existing values at that key
#' * get(key) - gets the value of a key, NULL otherwise
#' * unset(key) - unsets value of a key
#' * has(key) - true if the key has a value, false otherwise
#' * clear() - clears all values
#' @param
#' type name of the cache type
#' f function that returns a list of functions bound to a cache name
#' @return
#' list of functions that can be used to manipulate the named cache
#' @examples
#'
#' @export
##
addCacheType <- function(type, f) {

  # register the cache type
  assign(cacheTypeKey(type), f, envir=cache.env)
}

##
#' @title
#' Get a cache
#' @description
#' Gets a cache of the type specified taking into account any context provided
#' @param
#' name Cache name
#' type Type of cache
#' algo Hasing algorithm which defaults to sha1
#' @return
#'
#' @examples
#'
#' @export
##
cache <- function (type = "memory", algo="sha1") {

    #
    # Hash function
    #
    hash <- eval(bquote(
              function(value) {
                digest::digest(value, algo=.(algo))}))

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

      # remove memo args
      args.set[["force"]] <- NULL

      # get args hash
      hash(c(name, unlist(args.set, use.names = FALSE)))
    }

    # TODO maybe the name prefix should be managed here!

    # verify the cache type
    if (!exists(cacheTypeKey(type), envir=cache.env)) stop(paste("Undefined memo cache type:", type))

    # verify the cache name
    # TODO

    # store the created cache in memory for next time
    #cacheRegistry[[name]] <<-
    c(
      # create memo cache from type register
      #cacheTypeRegistry[[type]](name),
      get(cacheTypeKey(type), envir=cache.env)(),

      # append base cache methods and details
      list(type = type, algo = algo, hash = hash, hashFunctionCall = hashFunctionCall))
  # }
}
