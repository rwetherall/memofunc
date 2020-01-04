require(digest)

##
# Hash function.
#
# TODO make configurable
#
hash <- function(value) digest::digest(value)

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
unset.defaultArgs <- function (defaultArgs, args)
  defaultArgs[
    unlist(lapply(
        names(defaultArgs),
        function (name) {!name %in% names(args)}))]

##
# Hashes a function call based on the provided argument values and unspecified.
# 
hashFunctionCall <- function (name, formals, args)
  
  # get the default arguments
  formals %>% defaultArgs() %>% 
    
  # get the unset default arguments  
  unset.defaultArgs(args) %>%
  
  # combine the arguments with the unset default arguments  
  c(args) %>%
  
  # force the evaluation of the arguments  
  lapply(force) %>%
  
  # combine the argument values and the function name  
  unlist(use.names = FALSE) %>% c(name) %>% 
  
  # return hash  
  hash()
