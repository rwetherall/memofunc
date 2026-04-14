provider.azure.blob <- function(account, container, key = NULL, token = NULL, endpoint = "blob.core.windows.net", prefix = NULL) {
  if (!requireNamespace("AzureStor", quietly = TRUE)) {
    stop("AzureStor package is required for Azure Blob providers")
  }

  if (missing(account) || is.null(account) || account == "") {
    stop("account is required")
  }

  if (missing(container) || is.null(container) || container == "") {
    stop("container is required")
  }

  if (is.null(key) && is.null(token)) {
    stop("key or token is required")
  }

  if (!is.null(prefix) && prefix == "") {
    prefix <- NULL
  }

  if (!is.null(prefix)) {
    prefix <- sub("/+$", "", prefix)
  }

  endpoint_url <- sprintf("https://%s.%s", account, endpoint)
  storage_endpoint <- if (!is.null(token)) {
    AzureStor::storage_endpoint(endpoint_url, token = token)
  } else {
    AzureStor::storage_endpoint(endpoint_url, key = key)
  }

  container_client <- AzureStor::storage_container(storage_endpoint, container)

  key_blob <- function(key) {
    name <- paste0(digest::digest(key, algo = "sha1"), ".rds")
    if (is.null(prefix)) name else paste(prefix, name, sep = "/")
  }

  blob_exists <- function(blob) {
    exports <- getNamespaceExports("AzureStor")
    if ("storage_exists" %in% exports) {
      return(AzureStor::storage_exists(container_client, blob))
    }
    if ("blob_exists" %in% exports) {
      return(AzureStor::blob_exists(container_client, blob))
    }

    listing <- tryCatch(
      AzureStor::list_blobs(container_client, dir = blob),
      error = function(e) NULL
    )

    if (is.null(listing)) return(FALSE)
    if (is.data.frame(listing)) {
      if ("name" %in% names(listing)) return(any(listing$name == blob))
      if ("blob" %in% names(listing)) return(any(listing$blob == blob))
      if ("file" %in% names(listing)) return(any(listing$file == blob))
    }
    if (is.character(listing)) return(any(listing == blob))
    FALSE
  }

  list(
    put = function(key, value) {
      blob <- key_blob(key)
      tmp <- tempfile(fileext = ".rds")
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
      saveRDS(value, tmp)
      AzureStor::upload_blob(container_client, src = tmp, dest = blob, overwrite = TRUE)
      invisible(NULL)
    },
    get = function(key) {
      blob <- key_blob(key)
      if (!blob_exists(blob)) return(NULL)
      tmp <- tempfile(fileext = ".rds")
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
      AzureStor::download_blob(container_client, src = blob, dest = tmp, overwrite = TRUE)
      readRDS(tmp)
    },
    exists = function(key) {
      blob_exists(key_blob(key))
    },
    delete = function(key) {
      blob <- key_blob(key)
      if (blob_exists(blob)) {
        AzureStor::delete_blob(container_client, blob)
      }
      invisible(NULL)
    },
    clear = function() {
      listing <- tryCatch(
        AzureStor::list_blobs(container_client, dir = if (is.null(prefix)) "" else prefix),
        error = function(e) NULL
      )

      if (is.null(listing)) return(invisible(NULL))

      names <- NULL
      if (is.data.frame(listing) && "name" %in% names(listing)) {
        names <- listing$name
      } else if (is.data.frame(listing) && "blob" %in% names(listing)) {
        names <- listing$blob
      } else if (is.data.frame(listing) && "file" %in% names(listing)) {
        names <- listing$file
      } else if (is.character(listing)) {
        names <- listing
      }

      if (length(names) > 0) {
        for (blob in names) {
          AzureStor::delete_blob(container_client, blob)
        }
      }
      invisible(NULL)
    }
  )
}
