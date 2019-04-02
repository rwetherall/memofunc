library(digest)

# cache type registry
cacheTypeRegistry <- list()

# initialised cache registry
cacheRegistry <- list()

#
#
#
addCacheType <- function (type, f) {

  # todo .. some validation of the function provided

  cacheTypeRegistry[[type]] <<- f
}

hasCache <- function(name = "default") {

  name %in% names(cacheRegistry)
}

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
#' @export memoCache
##
# TODO .. make all the paramters mandatory (no don't so this .. see final)
# TODO .. add a memoCacheLookup method that takes just the name and fails if the cache isn't in the cache
# TODO .. this fails if you are trying to create a cache that is already in memory?!
# TODO .. rename to initialiseMemoCache??
initCache <- function (name = "default", type = "memory", algo="sha1") {

  # make sure the cache hasn't already been initialised
  if (hasCache(name)) {

    # already initialised a cache with this name
    stop(paste("Already initialised cache with name", name))

  } else {

    #
    # Hash function
    #
    # TODO .. need to dquote this?
    hash <- function(value) {
      digest(value, algo=algo)
    }

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

  # determine the name of the cache
  cacheName <- paste("memocache", name, sep="")
  cache <- as.symbol(cacheName)

  # create the cache storage if it doesn't already exist
  if (!exists(cacheName, sys.frame(0))) assign(cacheName, list(), envir = sys.frame(0))

  #
  # Set value for given key
  #
  set <- eval(bquote(function(key, value) {

    # if key already set remove so readded at the end of the list
    if (key %in% names(.(cache))) .(cache)[[key]] <<- NULL

    # set value
    .(cache)[[key]] <<- value

  }))

  # get value with key
  get <- eval(bquote(function(key) .(cache)[[key]]))

  # unset value with key
  unset <- eval(bquote(function(key) .(cache)[[key]] <<- NULL))

  # has value with key
  has <- eval(bquote(function(key) key %in% names(.(cache))))

  # clear all
  clear <- eval(bquote(function() {

    .(cache) <<- list()

  }))

  list (set = set, get = get, unset = unset, has = has, clear = clear)
})

# initialise default cache
if (!hasCache()) initCache()
