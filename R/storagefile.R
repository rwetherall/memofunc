storage.file.class = "file"

##
#' @export
##
storage.init.file <- function(storage.type = storage.file.class, base.dir = storage.file.default_dir(), ...) {
  provider <- provider.file.local(base.dir = base.dir)
  storage <- storage.init.object(provider = provider)
  storage$base.dir <- base.dir
  class(storage) <- c(storage.root.class, storage.file.class, storage.object.class)
  storage
}

storage.file.default_dir <- function() {
  base.dir <- NULL
  if (requireNamespace("tools", quietly = TRUE) && exists("R_user_dir", envir = asNamespace("tools"), inherits = FALSE)) {
    base.dir <- tools::R_user_dir("memofunc", "cache")
  }

  if (is.null(base.dir)) {
    base.dir <- file.path(path.expand("~"), ".memofunc", "cache")
  }

  base.dir
}

storage.file.key_path <- function(key, base.dir) {
  filename <- paste0(digest::digest(key, algo = "sha1"), ".rds")
  file.path(base.dir, filename)
}

provider.file.local <- function(base.dir = storage.file.default_dir()) {
  dir.create(base.dir, recursive = TRUE, showWarnings = FALSE)

  list(
    put = function(key, value) {
      saveRDS(value, storage.file.key_path(key, base.dir))
      invisible(NULL)
    },
    get = function(key) {
      path <- storage.file.key_path(key, base.dir)
      if (file.exists(path)) readRDS(path) else NULL
    },
    exists = function(key) file.exists(storage.file.key_path(key, base.dir)),
    delete = function(key) {
      path <- storage.file.key_path(key, base.dir)
      if (file.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
      invisible(NULL)
    },
    clear = function() {
      if (dir.exists(base.dir)) {
        files <- list.files(base.dir, full.names = TRUE, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
        if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
      }
      invisible(NULL)
    }
  )
}

