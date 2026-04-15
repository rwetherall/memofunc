library(magrittr)

memo.default_id <- function(f, f.symbol) {
  if (missing(f.symbol)) return(NULL)
  if (!is.symbol(f.symbol)) return(NULL)
  name <- as.character(f.symbol)
  if (length(name) != 1 || name == "") return(NULL)
  if (identical(name, "function")) return(NULL)
  if (grepl("^function", name)) return(NULL)
  name
}

memo.call_hash <- function(f, args) {
  formals(f) %>% defaultArgs() %>%
    unset.defaultArgs(args) %>% c(args) %>%
    orderby.name() %>%
    lapply(force) %>% lapply(hash) %>%
    hash()
}

memo.create <- function(f, allow.null = FALSE, id = NULL, function_hash_override = NULL) {
  # you can't memo a memo
  stopifnot(!is.memo(f))

  # TODO add option to prevent "force" and others being added to formals of memo function
  # TODO provide a way to supply cache arguments, for example the algo to use, max size, storage strategy, etc

  # get cache
  f.cache <- storage.init()

  # stable function token tied to function identity unless explicitly overridden
  f.function_hash <- if (is.null(function_hash_override)) hash(f) else hash(function_hash_override)

  # create the memo function
  f.memo <- function (memo.force=FALSE, memo.dryrun=FALSE) {

    # get the function call
    fc <- functionCall()

    # remove memo args we have added to the function
    fc$args %<>% removeby.name("memo.force") %>% removeby.name("memo.dryrun")

    # generate hash
    hash <- hash(list(id = id, function_hash = f.function_hash, call = memo.call_hash(f, fc$args)))

    # if force or cached
    if (!memo.force && storage.has(f.cache, hash)) {

      # false if dry run otherwise cached value
      if (memo.dryrun) FALSE else storage.get(f.cache, hash)

    } else {

      # TRUE if dry run otherwise call the function
      result <- if (memo.dryrun) TRUE else do.call(f, fc$args)

      # handle null results
      if (!is.null(result) || allow.null) {

        # cache the result
        if (!memo.dryrun) storage.set(f.cache, hash, result)
      }

      result
    }
  }

  # set the parameters and environment of the memo function
  formals(f.memo) <- c(formals(f), formals(f.memo))

  # set the class of the memo function
  class(f.memo) <- c("memo", class(f.memo))

  attr(f.memo, "memo.id") <- id
  attr(f.memo, "memo.function_hash") <- f.function_hash

  # return the memo function
  f.memo
}

##
#' @title Memo
#' @description
#' Creates a memoized function from a named or anonymous function. Calls to the
#' memoized function are served from cache after the first execution for a given
#' key.
#'
#' Runtime controls:
#' \itemize{
#'   \item \code{memo.force = TRUE}: ignore an existing cached value and recompute
#'   \item \code{memo.dryrun = TRUE}: do not execute or cache; return \code{TRUE} if execution would occur, \code{FALSE} if a cache hit exists
#' }
#'
#' By default, \code{NULL} results are not cached. Set \code{allow.null = TRUE} to
#' cache \code{NULL} values as valid outcomes.
#'
#' Hashing strategy for stored values:
#' \itemize{
#'   \item \strong{id component}: explicit \code{id} when provided, otherwise an inferred function name when available
#'   \item \strong{function component}: hash of the function formals and body (or \code{function_hash_override} when supplied)
#'   \item \strong{call component}: hash of normalized call arguments (including defaulted arguments, ordered by argument name)
#' }
#'
#' The final storage key is a hash over these three components. This means that,
#' by default, changing a function's implementation invalidates old cache entries
#' for future reads while leaving historical values in storage.
#'
#' Matching keys only share cached values when memo calls use the same underlying
#' storage. Separate in-memory memo instances do not share values, even when keys
#' are identical.
#' @param f function to memoise
#' @param id optional identifier used to scope cache keys; defaults to the
#' function name when available
#' @param function_hash_override optional override value used in place of the
#' default function identity hash
#' @param allow.null if \code{TRUE}, the memoized function caches \code{NULL}
#' results; \code{FALSE} by default
#' @return the memoed function
#' @example R/examples/memo/example.memo.R
#' @export
##
memo <- function (f, id = NULL, function_hash_override = NULL, allow.null=FALSE) {
  if (missing(id)) {
    id <- memo.default_id(f, substitute(f))
  }

  memo.create(f, allow.null = allow.null, id = id, function_hash_override = function_hash_override)
}

##
#' @title Is Memo
#' @description Checks whether the passed function is a memo function.
#' @param f function, memo or otherwise
#' @return \code{TRUE} if memo function, \code{FALSE} otherwise
#' @export
##
is.memo <- function(f) inherits(f, "memo")

##
#' @title Memo Cache
#' @description Gets the cache associated with a memo function allowing further manipulation and control of the underlying values
#' being stored.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Cache storing values for memoed function.
#' @export
##
memo.cache <- function(f) {

  stopifnot(is.memo(f))

  "f.cache" %>% get(envir=environment(f))
}

##
#' @title Memo Function
#' @description Gets the original function that was memoized.
#'
#' Execution is stopped if function passed is not a valid memoed function.
#' @param f memo function
#' @return Original unmemoized function.
#' @export
##
memo.function <- function(f) {

  stopifnot(is.memo(f))

  "f" %>% get(envir=environment(f))
}
