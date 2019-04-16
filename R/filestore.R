filestore <- function (dir=".") {

  before <- function(cacheId) {
    if (!dir.exists(dir)) dir.create(dir)
    if (!dir.exists(file.path(dir, cacheId))) dir.create(file.path(dir, cacheId))
  }

  write <- function (cacheId, key, value) {
    before(cacheId)
    saveRDS(value, file = file.path(dir, cacheId, key))
  }

  read <- function (key) {
    before(cacheId)
    readRDS(key, file=file.path(dir, cacheId, key))
  }

  list(write=write, read=read)
}
