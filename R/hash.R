require(digest)

##
# Hash function.
#
# TODO make configurable
#
hash <- function(value) digest::digest(value)

##
# is the value an empty name.
#
is.emptyName <- function (value) (value == "" && class(value) == "name")

##
# Gets the default values of a given set of function formals.
#
defaultArgs <- function(formals) formals[!sapply(formals, is.emptyName)]

##
# Hashes a function call based on the provided argument values and unspecified 
# defaults.
hashFunctionCall <- function(name, formals, args) {
  
  # get default args
  args.default <- defaultArgs(formals)
  
  # get the unset default args
  args.default.unset <- 
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args)}))]
  
  # evaluated set arguments
  args.set <- lapply(c(args, args.default.unset), force)
  
  # get args hash
  name %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
}

