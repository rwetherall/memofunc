library(digest)

# cache type registry
cacheTypeRegistry <- list()

# initialised cache registry
cacheRegistry <- list()

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
addCacheType <- function (type, f) {

  # todo .. some validation of the function provided

  cacheTypeRegistry[[type]] <<- f
}

##
#' @title
#' Has the named cache been initialised
#' @description
#' Indicates whether the named cache has been initialised or not.
#' @param
#' name name of the cache
#' @return
#' True if the named cache has been initialised, false otherwise
#' @examples
#'
#' @export
##
hasCache <- function(name = "default") {

  name %in% names(cacheRegistry)
}

##
#' @title
#' Get initialised cache by name
#' @description
#' Memoises a given function such that the result of the function is cached to improve
#' function performance
#' @param
#' name name of the cache
#' @return
#' Named cache if initialised, otherwise error
#' @examples
#'
#' @export
##
getCache <- function(name = "default") {

  if (hasCache(name))

    # return cache
    cacheRegistry[[name]]

  else

    # error since named cache hasn't been initialised
    stop(paste("Cache with name", name, "has not been initialised.  Please use 'initCache' method to initialise cache."))
}

##
#' @title
#' Get a reference to a named cache.
#' @description
#' Gets a reference to the named cache, creating if one if it doesn't exist.
#'
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
initCache <- function (name = "default", type = "memory", algo="sha1") {

  # make sure the cache hasn't already been initialised
  if (hasCache(name)) {

    # TODO do we really want to fail here .. or just find the init'ed cache?

    # already initialised a cache with this name
    stop(paste("Already initialised cache with name", name))

  } else {

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
    if (!type %in% names(cacheTypeRegistry)) stop(paste("Undefined memo cache type:", type))

    # verify the cache name
    # TODO

    # store the created cache in memory for next time
    cacheRegistry[[name]] <<- c(

      # create memo cache from type register
      cacheTypeRegistry[[type]](name),

      # append base cache methods and details
      list(name = name, type = type, algo = algo, hash = hash, hashFunctionCall = hashFunctionCall))
  }
}

#
# Default in-memory cache
#
addCacheType("memory", function(name) {

  cache <- list()

  #
  # Set value for given key
  #
  set <- function(key, value) {

    # if key already set remove so readded at the end of the list
    if (key %in% cache) cache[[key]] <<- NULL

    # set value
    cache[[key]] <<- value
  }

  # get value with key
  get <- function(key) cache[[key]]

  # unset value with key
  unset <- function(key) cache[[key]] <<- NULL

  # has value with key
  has <- function(key) key %in% names(cache)

  # clear all
  clear <- function() cache <<- list()

  list (set = set, get = get, unset = unset, has = has, clear = clear)
})
