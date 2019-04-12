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
memo <- function (f, cache) {

  if (is.null(cache)) error("A cache is required when memoising a function.")

  # grab the information about the function
  f.formals <- formals(f)

  # create the memo function
  f.memo <- function (force=FALSE) {

    # grab the call
    call <- match.call()

    # remove force from call if present
    if (!is.null(call[["force"]])) call[["force"]] <- NULL

    # generate hash from function name and arguments
    hash <- cache$hashFunctionCall(call[[1]], formals(), as.list(call[-1]))

    # if force or cached
    if (!force && cache$has(hash)) {

      # returned cached value
      cache$get(hash)

    } else {

      # tweak call to call f and remove force
      call[[1]] <- quote(f)

      # evaluate call and cache results
      cache$set(hash, eval(call))
    }
  }

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(f.formals, formals(f.memo))

  # return the memo function
  f.memo
}
