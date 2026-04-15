##
#' @title Initialize a store.
#' @description 
#' Initlaize storage for name value pairs based on provided type.
#' 
#' Available types of storage include:
#' \itemize{
#'    \item \code{memory}: transient in-memory storage
#'    \item \code{file}: persistent storage, using local file storage
#'    \item \code{object}: provider-backed object storage
#' }
#'
#' Use \code{file} when you want a simple local file store without provider configuration.
#' Use \code{object} with a provider (e.g. \code{file}, \code{azure.blob}) when you want
#' a consistent, pluggable interface across storage backends. This allows the same code
#' path to swap between local and cloud providers.
#' 
#' Additional paramters may be provided when initializing different types of storage.
#' If \code{storage.type} is not provided and \code{memofunc.storage.provider} is set,
#' then the provider is used to initialize storage.
#'
#' Providers are resolved by name (for example \code{file} or \code{azure.blob}), with
#' synonyms such as \code{local} or \code{azure} mapping to their canonical names. The
#' Azure provider requires the \code{AzureStor} package.
#' 
#' See specific storage types for details.
#' @param storage.type storage type to initialize, defaults to \code{memory}
#' @param provider optional provider name or provider configuration
#' @param ... additional configuration values used by storage implementations
#' @return List containing characteristics perticular to the storage implementation, including:
#' \itemize{
#'    \item \code{type}: the storage type field
#' }
#' @example R/examples/storage/example.storage.R
#' @export
##
storage.init <- function(storage.type = storage.memory.class, provider = getOption("memofunc.storage.provider", NULL), ...)
  if (missing(storage.type) && !is.null(provider)) {
    storage.init_from_provider(provider = provider, ...)
  } else {
    UseMethod("storage.init", (obj <- list()) %>% `class<-`(storage.type))
  }


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

storage.root.class = "storage"

storage.init_from_provider <- function(provider, ...) {
  if (storage.provider.is_provider(provider)) {
    return(storage.init.object(provider = provider))
  }

  if (is.list(provider) && !is.null(provider$name)) {
    provider.name <- provider$name
    provider.config <- provider$config
    if (is.null(provider.config)) {
      provider.config <- list()
    }
    provider.args <- provider
    provider.args$name <- NULL
    provider.args$config <- NULL
    return(do.call(storage.init_from_provider, c(list(provider = provider.name), provider.config, provider.args, list(...))))
  }

  if (is.character(provider) && length(provider) == 1) {
    if (provider == storage.file.class) {
      return(do.call(storage.init.file, list(...)))
    }

    return(storage.init.object(provider = provider, ...))
  }

  stop("provider must be a provider name, provider config, or provider implementation")
}

storage.new <- function() {
  
}

`storage.class<-` <- function(storage, value)
  `class<-`(storage, c(storage.root.class, value))
  
