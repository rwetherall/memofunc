#
# Default in-memory cache
#
memoryStorage <- function() {

  # Environment used as in memory cache storage
  cache.storage <- new.env(parent=emptyenv())

  # Set value for given key
  set <- function(key, value) assign(key, value, envir=cache.storage)

  # get value with key
  get <- function(key) if (exists(key, envir=cache.storage)) base::get(key, envir=cache.storage) else NULL

  # unset value with key
  unset <- function(key) if (exists(key, envir=cache.storage)) rm(list=c(key), envir=cache.storage)

  # has value with key
  has <- function(key) exists(key, envir=cache.storage)

  # clear all
  clear <- function() rm(list=base::ls(cache.storage), envir=cache.storage)

  # list cache contents
  ls <- function() base::ls(cache.storage)

  list (set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
