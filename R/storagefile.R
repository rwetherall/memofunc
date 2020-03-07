##
#' @title File Storage Set
#' @description 
#' Sets a value into the file storage.
#' @param cache cache 
#' @param key key value
#' @param value value
#' @return set value
#' @export
##
storage.set.file <- function (cache, key, value)
  NULL


##
#' @title File Storage Get
#' @description 
#' Gets a value from the file storage.
#' @param cache cache
#' @param key key value
#' @return value, NULL if not set
#' @export
##
storage.get.file <- function(cache, key) 
  NULL

##
#' @title File Storage Unset
#' @description 
#' Unsets a value from the file storage.
#' @param cache cache
#' @param key key value
#' @export
##
storage.unset.file <- function(cache, key) 
  NULL

##
#' @title File Storage Has
#' @description 
#' Determines whether a key is set in the file storage.
#' @param cache cache
#' @param key key value
#' @return TRUE if the storage has the key, FALSE otherwise
#' @export
##
storage.has.file <- function(cache, key) 
  NULL

##
#' @title File Storage Clear
#' @description 
#' Clears the file storage of all values.
#' @param cache cache
#' @export
##
storage.clear.file <- function(cache) 
  NULL