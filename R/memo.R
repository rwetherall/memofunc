##
#' @title
#' Memoises a given function
#' @description
#' Memoises a given function such that the result of the function is cached to improve
#' function performance
#' @param f Function to memoise.
#' @param cache Cache to store functions memoised results in. If not specified create a new non-persistant memory cache.
#' @return
#' Memoised function
#' @examples
#' # example function
#' example <- function (someValue) {
#'   print("Getting some value")
#'   someValue
#' }
#'
#' # memoise function
#' memoExample <- memo(example, cache())
#'
#' # call first time
#' memoExample(10)  # prints "Getting some value" and returns 10
#'
#' # call second time
#' memoExample(10)  # immediately returns
#' @export
##
memo <- function (f, cache) {

  # Hash function
  hash <- function(value) digest::digest(value)

  # Helper to create hash from function and args details.
  hashFunctionCall <- function(name, formals, args) {

    # get default args
    args.default <- formals[
      unlist(
        lapply(formals,
               function(arg) {arg != ""}))]

    # get unset default args
    args.set <- c(
      args,
      args.default[
        unlist(
          lapply(names(args.default),
                 function(name) {!name %in% names(args)}))])

    # get args hash
    hash(c(name, unlist(args.set, use.names = FALSE)))
  }

  # grab the information about the function
  f.formals <- formals(f)

  # create the memo function
  f.memo <- function (force=FALSE) {

    # grab the call
    call <- match.call()

    # remove force from call if present
    if (!is.null(call[["force"]])) call[["force"]] <- NULL

    # generate hash from function name and arguments
    hash <- hashFunctionCall(call[[1]], formals(), as.list(call[-1]))

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

#TODO dememo - removes all values from the cache and rm's the assigned function to prevent it being called again!
