storage.object.class = "object"

##
#' @title Initialize an object store.
#' @description
#' Initializes object storage backed by a provider.
#'
#' The provider must supply methods for putting, getting, checking, deleting,
#' and clearing stored values.
#' @inherit storage.init
#' @param provider provider implementation exposing put/get/exists/delete/clear
#' @export
##
storage.init.object <- function(storage.type = storage.object.class, provider, ...) {
  if (missing(provider)) stop("provider is required")

  provider <- storage.provider.resolve(provider, ...)

  required <- storage.provider.required
  missing_methods <- setdiff(required, names(provider))
  if (length(missing_methods) > 0) {
    stop(sprintf("provider is missing methods: %s", paste(missing_methods, collapse = ", ")))
  }

  not_functions <- required[!vapply(provider[required], is.function, logical(1))]
  if (length(not_functions) > 0) {
    stop(sprintf("provider methods must be functions: %s", paste(not_functions, collapse = ", ")))
  }

  list(
    provider = provider
  ) %>% `storage.class<-`(storage.object.class)
}

storage.provider.required <- c("put", "get", "exists", "delete", "clear")

storage.provider.registry <- list(
  file = "provider.file.local",
  "azure.blob" = "provider.azure.blob"
)

storage.provider.synonyms <- list(
  local = "file",
  azure = "azure.blob",
  azureblob = "azure.blob",
  blob = "azure.blob"
)

storage.provider.normalize <- function(name) {
  if (!is.character(name) || length(name) != 1) return(name)
  synonym <- storage.provider.synonyms[[name]]
  if (is.null(synonym)) name else synonym
}

storage.provider.is_provider <- function(provider) {
  is.list(provider) && all(storage.provider.required %in% names(provider))
}

storage.provider.resolve <- function(provider, ...) {
  if (storage.provider.is_provider(provider)) {
    return(provider)
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
    return(do.call(storage.provider.resolve, c(list(provider.name), provider.config, provider.args, list(...))))
  }

  if (is.character(provider) && length(provider) == 1) {
    provider.name <- storage.provider.normalize(provider)
    provider.factory_name <- storage.provider.registry[[provider.name]]
    if (!is.null(provider.factory_name)) {
      provider.factory <- get(provider.factory_name, mode = "function")
      return(do.call(provider.factory, list(...)))
    }
  }

  stop("provider must be a provider name, provider config, or provider implementation")
}

##
#' @title Set value into an object store.
#' @inherit storage.set
#' @export
##
storage.set.object <- function(storage, key, value) {
  storage$provider$put(key, value)
  invisible(storage)
}

##
#' @title Get a value from an object store.
#' @inherit storage.get
#' @export
##
storage.get.object <- function(storage, key) storage$provider$get(key)

##
#' @title Unset a value that corresponds to a key within an object store.
#' @inherit storage.unset
#' @export
##
storage.unset.object <- function(storage, key) {
  storage$provider$delete(key)
  invisible(storage)
}

##
#' @title Has key has been used to store a value in an object store?
#' @inherit storage.has
#' @export
##
storage.has.object <- function(storage, key) storage$provider$exists(key)

##
#' @title Clear the object store.
#' @inherit storage.clear
#' @export
##
storage.clear.object <- function(storage) {
  storage$provider$clear()
  invisible(storage)
}
