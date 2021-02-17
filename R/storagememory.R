storage.init.default <- function (storage.type, ...) 
  list(
    type = "memory",
    env = new.env(parent=emptyenv())
  ) %>% `class<-`("storage")


storage.set.default <- function (storage, key, value){
  assign(key, value, envir=storage$env)
  invisible(storage)
}

storage.get.default <- function(storage, key) 
  if (exists(key, envir=storage$env)) base::get(key, envir=storage$env) else NULL

storage.unset.default <- function(storage, key) 
  if (exists(key, envir=storage$env)) rm(list=c(key), envir=storage$env)

storage.has.default <- function(storage, key) exists(key, envir=storage$env)

storage.clear.default <- function(storage) 
  rm(list=base::ls(storage$env), envir=storage$env)

