##
#' @title Initialize a memory store.
#' @description 
#' Initlaize memory storage, used to hold and retrieve values in memory.
#' 
#' The storage type is expected to specified as \code{memory}.
#' 
#' This storage is transient.  
#' @inherit storage.init
#' @export
##
storage.init.default <- function (storage.type, ...) 
  list(
    type = "memory",
    env = new.env(parent=emptyenv())
  ) %>% `class<-`("storage")

##
#' @title Set value into a memory store.
#' @inherit storage.set
#' @export
##
storage.set.default <- function (storage, key, value){
  assign(key, value, envir=storage$env)
  invisible(storage)
}

##
#' @title Get a value from a memory store.
#' @inherit storage.get
#' @export
##
storage.get.default <- function(storage, key) 
  if (exists(key, envir=storage$env)) base::get(key, envir=storage$env) else NULL

##
#' @title Unset a value that corresponds to a key within a memory store.
#' @inherit storage.unset
#' @export
##
storage.unset.default <- function(storage, key) {
  if (exists(key, envir=storage$env)) rm(list=c(key), envir=storage$env)
  invisible(storage)
}

##
#' @title Has key has been used to store a value in a memory store?
#' @inherit storage.has
#' @export
##
storage.has.default <- function(storage, key) exists(key, envir=storage$env)

##
#' @title Clear the memory store.
#' @inherit storage.clear
#' @export
##
storage.clear.default <- function(storage) {
  rm(list=base::ls(storage$env), envir=storage$env)
  invisible(storage)
}

