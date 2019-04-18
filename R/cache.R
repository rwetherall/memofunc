library(digest)
library(uuid)

##
#' @title Create a cache.
#' @description Create a general purpose cache that can be used to store both transient and persistent values.
#' @param id Cache identifier, used to re-create reference to exisiting persistent cache.  If none provided then assumed transient.
#' @param storetype Persistant cache storage type.  File store by default.
#' @return A cache that stores values for later retieval.
#' @export
##
cache <- function (id = NULL, storetype = "filestore") {

  # if id is specified then get the store
  store <- if (!is.null(id)) eval(as.name(storetype))() else NULL

  # generate uuid to indentify this cache
  if (is.null(id)) id <- UUIDgenerate()

  # environment used as memory storage
  memoryStorage <- new.env(parent=emptyenv())

  # Set value for given key
  set <- function(key, value) {

    if (!is.null(store)) store$write(id, key, value)

    # store value in memory
    assign(key, value, envir=memoryStorage)

    # return value since assign return is invisible
    value
  }

  # get value with key
  get <- function(key) {

    # check memory first
    if (exists(key, envir=memoryStorage)) {

      # get value from memory
      base::get(key, envir=memoryStorage)
    }
    else if (!is.null(store)) {

      # read and set cache value
      set(key, store$read(id, key))
    }
  }

  # unset value with key
  unset <- function(key) {

    # remove for memory
    if (exists(key, envir=memoryStorage)) rm(list=c(key), envir=memoryStorage)

    # remove from storage
    if (!is.null(store)) store$delete(id, key)
  }

  # has value with key
  has <- function(key) !is.null(get(key))

  # clear all
  clear <- function() {

    # clear memory
    rm(list=base::ls(memoryStorage), envir=memoryStorage)

    # clear store
    if (!is.null(store)) store$clear(id)
  }

  # list cache contents
  ls <- function(memoryOnly = TRUE) {

    if (memoryOnly) base::ls(memoryStorage) # else TODO list from storage
  }

  # combine cache and storage features
  list(id=id, set = set, get = get, unset = unset, has = has, clear = clear, ls=ls)
}
