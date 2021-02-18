#' @title memofunc: A package for memoizing functions and caching data
#' @author Roy Wetherall \email{rwetherall@gmail.com}
#' @description
#' The \code{memofunc} package provides a simple way to memoize a function to optimise execution for process or data
#' intensive actions.
#' @section Memoization Functions:
#' \itemize{
#'   \item \code{\link{memo}} - memoize a function
#'   \item \code{\link{is.memo}} - is the given function a memo
#'   \item \code{\link{memo.function}} - get a memo's origional function
#'   \item \code{\link{memo.cache}} - get a memo's cache storage
#' }
#' @docType package
#' @name memofunc
#'
#' @import magrittr
#' @import uuid
#' @import digest
NULL
