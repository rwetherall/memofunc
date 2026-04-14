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
  if (!is.list(provider)) stop("provider must be a list")

  required <- c("put", "get", "exists", "delete", "clear")
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
