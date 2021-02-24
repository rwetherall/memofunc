##
#' @title Initialize a store.
#' @description 
#' Initlaize storage for name value pairs based on provided type.
#' 
#' Available types of storage include:
#' \itemize{
#'    \item{\code{memory} - transient in-memory storage}
#'    \item{\code{file} - persistent storage, using local file storage}
#' }
#' 
#' Additional paramters may be provided when initializing different types of storage.  
#' 
#' See specific storage types for details.
#' @param storage.type storage type to initialize, defaults to \code{memory}
#' @param ... additional configuration values used by storage implementations
#' @return List containing characteristics perticular to the storage implementation, including:
#' \itemize{
#'    \item{\code{$type} - the storage type}
#' }
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.init <- function(storage.type = storage.memory.class, ...)
  UseMethod("storage.init", (obj <- list()) %>% `class<-`(storage.type) )


##
#' @title Set value into a store.
#' @description 
#' Stores a value for a given key.
#' 
#' If there is already a value stored for the key provided, then the exisiting value is 
#' overriden with the new value.
#' @param storage initialized storage
#' @param key key to store value against
#' @param value value to store
#' @return Invisbily returns storage
#' @example R/examples/storage/example.storage.R
#' @export
storage.set <- function (storage, key, value) UseMethod("storage.set", storage)

##
#' @title Get value from a store.
#' @description 
#' Gets a value, for a given key, from the store.
#'  
#' If there is no coresponding value for the key, then \code{NULL} is returned.
#' @param storage initialized storage
#' @param key key to retrieve value for
#' @return Stored value for the key, \code{NULL} otherwise.
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.get <- function (storage, key) UseMethod("storage.get", storage)

##
#' @title Unset a value that corresponds to a key within a store.
#' @description 
#' Unsets the value stored for a given key.
#' 
#' If there is no value for the key provided no action is taken.
#' @param storage initialized storage
#' @param key key whose value is to be unset
#' @return Invisibily returns storage
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.unset <- function (storage, key) UseMethod("storage.unset", storage)

##
#' @title Has key has been used to store a value?
#' @description
#' Indicates if a given key has a associated value stored in the storage or not.
#' @param storage initialized storage
#' @param key key to check for stored value
#' @return \code{TRUE} if key has an associated stored value, \code{FALSE} otherwise.
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.has <- function (storage, key) UseMethod("storage.has", storage)

##
#' @title Clear the storage.
#' @description 
#' Clear the given storage of all keys and their values.
#' @param storage initialized storage
#' @return Invisibily returns storage
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.clear <- function (storage) UseMethod("storage.clear", storage)