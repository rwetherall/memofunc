#' @title memofunc: A package for memoizing functions and caching data
#' @author Roy Wetherall \email{rwetherall@gmail.com}
#' @description
#' The \code{memofunc} package provides tools for function memoization with
#' explicit cache key semantics and pluggable storage backends.
#' @section Core Concepts:
#' \itemize{
#'   \item \strong{Memoized functions}: cached calls still behave like normal R functions
#'   \item \strong{Deterministic keys}: cache keys combine function identity and normalized call arguments
#'   \item \strong{Configurable storage}: memory, file, and object provider backends
#'   \item \strong{Runtime controls}: force recomputation or perform dry-run cache checks
#' }
#' @section Memoization Functions:
#' \itemize{
#'   \item \code{\link{memo}} - memoize a function
#'   \item \code{\link{is.memo}} - is the given function a memo
#'   \item \code{\link{memo.function}} - get a memo's origional function
#'   \item \code{\link{memo.cache}} - get a memo's cache storage
#' }
#' @import magrittr
#' @import uuid
#' @import digest
#' @keywords internal
"_PACKAGE"
