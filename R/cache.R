#' @include storagememory.R
NULL

library(digest)
library(uuid)
library(magrittr)

##
#' @title Create Cache
#' @description 
#' Creates a cache that can be used to store name, value pairs.
#' 
#' The provided storage class indicates where the values in the cache are stored for it's duration.
#' @param storage storage class
#' @return a new cache
#' @example R/examples/cache/example.cache.R
#' @export
##
cache <- function (storage = "memory")
  list(
    memory_cache = new.env(parent=emptyenv()),
    storage = storage %>% `class<-`(storage)
  ) %>% `class<-`("cache")

##
#' @title Cache Get
#' @description 
#' Gets a value from the cache.
#' @param cache cache
#' @param key value key
#' @return value, NULL otherwise
#' @example R/examples/cache/example.cache.R
#' @export
##
cache.get <- function(cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  storage.get(cache, key)
}

##
#' @title Cache Set
#' @description 
#' Sets a value into the cache.
#' @param cache cache
#' @param key value key
#' @param value value
#' @return the cache
#' @example R/examples/cache/example.cache.R
#' @export
##
cache.set <- function (cache, key, value) {

  stopifnot(inherits(cache, "cache"))
  storage.set(cache, key, value)
}

##
#' @title Cache Unset
#' @description 
#' Unsets a value from the cache.
#' @param cache cache 
#' @param key value key
#' @example R/examples/cache/example.cache.R
#' @export
##
cache.unset <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  storage.unset(cache, key)
}

##
#' @title Cache Has
#' @description 
#' Determines whether the key is in the cache.
#' @param cache cache
#' @param key value key
#' @return TRUE if the cache contains the key, FALSE otherwise
#' @example R/examples/cache/example.cache.R
#' @export
##
cache.has <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  storage.has(cache, key) 
}

##
#' @title Cache Clear
#' @description 
#' Clears all the values from the cache.
#' @param cache cache
#' @param key value key
#' @example R/examples/cache/example.cache.R
#' @export
##
cache.clear <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  storage.clear(cache)
}


