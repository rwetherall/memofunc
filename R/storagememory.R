
storage.set <- function (cache, key, value) UseMethod("storage.set", cache$storage)

storage.set.default <- function (cache, key, value) {
  assign(key, value, envir=cache$memory_cache)
  cache
}

storage.get <- function (cache, key) UseMethod("storage.get", cache$storage)

storage.get.default <- function(cache, key) 
  if (exists(key, envir=cache$memory_cache)) base::get(key, envir=cache$memory_cache) else NULL

storage.unset <- function (cache, key) UseMethod("storage.unset", cache$storage)

storage.unset.default <- function(cache, key) 
  if (exists(key, envir=cache$memory_cache)) rm(list=c(key), envir=cache$memory_cache)

storage.has <- function (cache, key) UseMethod("storage.has", cache$storage)

storage.has.default <- function(cache, key) exists(key, envir=cache$memory_cache)

storage.clear <- function (cache) UseMethod("storage.clear", cache$storage)

storage.clear.default <- function(cache) 
  rm(list=base::ls(cache$memory_cache), envir=cache$memory_cache)

