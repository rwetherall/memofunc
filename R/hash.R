require(digest)
require(magrittr)

source("./R/helper.R")

##
# Get a list of the formal names that have not been used in the provided argument list
#
unused.formals <- function (formals, args) formals[!sapply(names(formals), `%in%`, table=names(args))] %>% names()

##
# Get all the names of a list of arguments given a functions formals
#
all.names <- function (formals, args) {
  
  # exclude elip and get all formals that haven't been named in the argument list
  unused.formals <- removeby.name(formals, "...") %>% unused.formals(args)
  
  # argument names used
  names.args <- names(args)
  
  # if no argument names are set
  if (is.null(names.args)) {
    
    # return the argument names based on those provided in the formals
    pad(unused.formals, length(args), "mf.na")
  
  } else {
    
    # which argument names are not set
    mask <- sapply(names.args, `==`, y = "")
    
    # set the argument names that haven't been set
    if (any(mask)) names.args[mask] <- pad(unused.formals, length(names.args[mask]), "mf.na")
    
    # return all the argument names
    names.args
  }
}

##
#' @title Function Call
#' @description 
#' For a given function and call, return a list of class 'functionCall' which
#' can be hashed to provide a unique identifier for the function and parameters used for this call.
#' @param f function, defaults to the containing function
#' @param call call, default to the containing call
#' @return functionCall, a hashable form of the function call information
#' @example examples/hash/example.functionCall.R
#' @export
##
functionCall <- function (f = sys.function(sys.parent()), call = sys.call(sys.parent())) {
  
  # function call arguments
  args <- rest(as.list(call))
  
  # name all the function call arguments
  names(args) <- all.names(formals(f), args)
  
  # return functionCall
  list(f = f, args = args) %>% `class<-`("functionCall")
}

## TODO add algo as optional parameter, allow system default to be set

##
#' @title Hash
#' @description
#' Hashes a value into a string.
#' @param value value to hash
#' @return hashed value as a string
#' @export
##
hash <- function (value) UseMethod("hash", value)

##
#' @inherit hash
#' @export
##
hash.default <- function (value) digest::digest(value)

##
#' @inherit hash
#' @export
##
hash.function <- function (value) hash.default(body(value))

##
# Is the value an empty name.
#
is.emptyName <- function (value) (value == "" && class(value) == "name")

##
# Gets the default values of a given set of function formals.
#
defaultArgs <- function (formals) formals[!sapply(formals, is.emptyName)]

##
# Gets the unset default arguments. 
#
unset.defaultArgs <- function (defaultArgs, args) defaultArgs[!sapply(names(defaultArgs), `%in%`, table=names(args))]

##
#' @inherit hash
#' @export
##
hash.functionCall <- function (value) {
  
  # get functions default arguments
  formals(value$f) %>% defaultArgs() %>%
  
  # add the unset default arguments to the argument list
  unset.defaultArgs(value$args) %>% c(value$args) %>%
  
  # order arguments by name
  orderby.name() %>%
  
  # force the values and hash, this ensures that things like functions are comparable in a consistant way
  lapply(force) %>% lapply(hash) %>%
  
  # add the hash of original function
  c(hash(value$f)) %>%

  # hash the function call
  hash() 
}
