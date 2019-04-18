filestore <- function (dir=tempdir()) {

  ##
  # TODO documentation
  ##
  before <- function(cacheId) {
    if (is.null(cacheId)) stop("Invalid Parameter: cacheId must be specified.")
    if (!dir.exists(dir)) dir.create(dir)
    if (!dir.exists(file.path(dir, cacheId))) dir.create(file.path(dir, cacheId))
  }

  value.path <- function(cacheId, key) file.path(dir, cacheId, key)

  ##
  # TODO documentation
  ##
  write <- function (cacheId, key, value) {
    before(cacheId)
    saveRDS(value, file=value.path(cacheId, key))
  }

  ##
  # TODO documentation
  ##
  read <- function (cacheId, key) {

    before(cacheId)

    if (file.exists(value.path(cacheId, key))) readRDS(file=value.path(cacheId, key)) else NULL
  }

  delete <- function (cacheId, key) {

    if (file.exists(value.path(cacheId, key))) unlink(value.path(cacheId, key))
  }

  clear <- function (cacheId) {
    if (dir.exists(file.path(dir, cacheId))) unlink(file.path(dir, cacheId), recursive=TRUE, force=TRUE)
  }

  list(write=write, read=read, delete=delete, clear=clear)
}
