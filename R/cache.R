library(digest)
library(uuid)
require(magrittr)

##
#' @title Create Cache
#' @description 
#' Creates a cache that can be used to store name, value pairs.
#' 
#' The provided storage class indicates where the values in the cache are stored for it's duration.
#' @param storage storage class
#' @return a new cache
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
#' @export
##
cache.get <- function(cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  UseMethod("storage.get", cache$storage)
}

##
#' @title Cache Set
#' @description 
#' Sets a value into the cache.
#' @param cache cache
#' @param key value key
#' @param value value
#' @return set value
#' @export
##
cache.set <- function (cache, key, value) {

  stopifnot(inherits(cache, "cache"))
  UseMethod("storage.set", cache$storage)
}

##
#' @title Cache Unset
#' @description 
#' Unsets a value from the cache.
#' @param cache cache 
#' @param key value key
#' @export
##
cache.unset <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  UseMethod("storage.unset", cache$storage)
}

##
#' @title Cache Has
#' @description 
#' Determines whether the key is in the cache.
#' @param cache cache
#' @param key value key
#' @return TRUE if the cache contains the key, FALSE otherwise
#' @export
##
cache.has <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  UseMethod("storage.has", cache$storage) 
}

##
#' @title Cache Clear
#' @description 
#' Clears all the values from the cache.
#' @param cache cache
#' @param key value key
#' @export
##
cache.clear <- function (cache, key) {
  
  stopifnot(inherits(cache, "cache"))
  UseMethod("storage.clear", cache$storage)
}
