storage.file.class = "file"

##
#' @export
##
storage.init.file <- function(storage.type = storage.file.class, ...)
  list(
    memory = storage.init.memory()
  ) %>% `class<-`(c(storage.root.class, storage.file.class))

