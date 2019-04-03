##
#' @title
#' Memoises a given function
#' @description
#' Memoises a given function such that the result of the function is cached to improve
#' function performance
#' @param
#' f function to memoise
#' @return
#' Memoised function
#' @examples
#' # example function
#' example <- function (someValue) {
#'    Sys.sleep(someValue)
#'    somevalue
#' }
#'
#' # memoise function
#' memoExample <- memo(example)
#'
#' # call first time
#' memoExample(10)  # sleeps for 10 seconds and returns 10
#'
#' # call second time
#' memoExample(10)  # immediately returns 10
#' @export
##
memo <- function (f, cache = NULL) {

  # grab the information about the function
  f.formals <- formals(f)
  f.env <- environment(f)

  # create the memo function
  f.memo <-
    eval(
      bquote(
        function () {

          # get default cache if non specified
          if (is.null(.(cache))) {
            if (!hasCache()) initCache()
            cache = getCache()
          }

          # get args hash
          args <- as.list(match.call())
          hash <- cache$hashFunctionCall(args[1], formals(), args[-1])

          # get value from cache or execute
          if (!force && cache$has(hash)) cache$get(hash) else cache$set(hash, .(body(f)))
  }))

  # additional memo related properties to add to memoed function
  memo.formals <- list(force=FALSE)

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(f.formals, memo.formals)
  environment(f.memo) <- f.env

  # return the memo function
  f.memo
}
