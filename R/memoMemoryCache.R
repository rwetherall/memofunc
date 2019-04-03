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
