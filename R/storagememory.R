##
#' @export
##
storage.init.default <- function (storage.type, ...) 
  list(
    type = "memory",
    env = new.env(parent=emptyenv())
  ) %>% `class<-`("storage")

##
#' @export
##
storage.set.default <- function (storage, key, value){
  assign(key, value, envir=storage$env)
  invisible(storage)
}

##
#' @export
##
storage.get.default <- function(storage, key) 
  if (exists(key, envir=storage$env)) base::get(key, envir=storage$env) else NULL

##
#' @export
##
storage.unset.default <- function(storage, key) {
  if (exists(key, envir=storage$env)) rm(list=c(key), envir=storage$env)
  invisible(storage)
}

##
#' @export
##
storage.has.default <- function(storage, key) exists(key, envir=storage$env)

##
#' @export
##
storage.clear.default <- function(storage) {
  rm(list=base::ls(storage$env), envir=storage$env)
  invisible(storage)
}

