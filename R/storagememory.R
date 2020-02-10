##
#' @title Memory Storage Set
#' @description 
#' Sets a value into the memory storage.
#' @param cache cache 
#' @param key key value
#' @param value value
#' @return set value
#' @export
##
storage.set.memory <- function (cache, key, value)
  assign(key, value, envir=cache$memory_cache) %>% return()


##
#' @title Memory Storage Get
#' @description 
#' Gets a value from the memory storage.
#' @param cache cache
#' @param key key value
#' @return value, NULL if not set
#' @export
##
storage.get.memory <- function(cache, key) 
  if (exists(key, envir=cache$memory_cache)) base::get(key, envir=cache$memory_cache) else NULL

##
#' @title Memory Storage Unset
#' @description 
#' Unsets a value from the memory storage.
#' @param cache cache
#' @param key key value
#' @export
##
storage.unset.memory <- function(cache, key) 
  if (exists(key, envir=cache$memory_cache)) rm(list=c(key), envir=cache$memory_cache)

##
#' @title Memory Storage Has
#' @description 
#' Determines whether a ket is set in the memory storage.
#' @param cache cache
#' @param key key value
#' @return TRUE if the storage has the key, FALSE otherwise
#' @export
##
storage.has.memory <- function(cache, key) exists(key, envir=cache$memory_cache)

##
#' @title Memory Storage Clear
#' @description 
#' Clears the memory storage of all values.
#' @param cache cache
#' @export
##
storage.clear.memory <- function(cache) 
  rm(list=base::ls(cache$memory_cache), envir=cache$memory_cache)