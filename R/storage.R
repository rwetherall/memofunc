storage.init <- function(storage.type = "memory", ...) UseMethod("storage.init", storage.type)

storage.set <- function (storage, key, value) UseMethod("storage.set", storage$type)

storage.get <- function (storage, key) UseMethod("storage.get", storage$type)

storage.unset <- function (storage, key) UseMethod("storage.unset", storage$type)

storage.has <- function (storage, key) UseMethod("storage.has", storage$type)

storage.clear <- function (storage) UseMethod("storage.clear", storage$type)