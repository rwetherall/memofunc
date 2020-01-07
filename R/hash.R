require(digest)

##
# Helper to get the first N elements of a vector or list
#
first.n <- function (x, n) x[c(1:n)]
`%<n>%` <- function (x, n) first.n(x,n)

##
# Helper to pad a list 
#
pad <- function (x, n, by=NA) lapply(1:n, function (index) if (index > length(x)) x[[index]] <- by else x[[index]])

##
# Get a list of the formal names that have not been used in the provided argument list
#
unused.formals <- function (formals, args) formals[!sapply(names(formals), `%in%`, table=names(args))] %>% names()

##
# Get all the names of a list of arguments given a functions formals
#
all.names <- function (formals, args) {
  
  # remove the elip from the list of formals if it is present
  if ("..." %in% names(formals)) formals[["..."]] <- NULL
  
  # formals not named in the argument list
  unused.formals <- unused.formals(formals, args)
  
  # argument names used
  names.args <- names(args)
  
  # if no argument names are set
  if (is.null(names.args)) {
    
    # return the argument names based on those provided in the formals
    pad(unused.formals, length(args), "mf.na")
  
  } else {
    
    # which argument names are not set
    mask <- sapply(names.args, `==`, y="")
    
    # set the argument names that haven't been set
    if (any(mask)) names.args[mask] <- pad(unused.formals, length(names.args[mask]), "mf.na")
    
    # return all the argument names
    names.args
  }
}

##
# Get a 'functionCall', which is a hash'able class containing information about a call to a function
#
functionCall <- function (f = sys.function(sys.parent()), call = sys.call(sys.parent())) {
  
  # function call name
  name <- call[[1]]
  
  # function call arguments
  args <- as.list(call)
  args[[1]] <- NULL
  
  # name all the function call arguments
  names(args) <- all.names(formals(f), args)
  
  # return functionCall
  result <- list(f=f, name=name, args=args)
  class(result) <- "functionCall"
  result
}

##
# Generic hash function
#
hash <- function (value) UseMethod("hash", value)

##
# Default hash function
#
hash.default <- function (value) digest::digest(value)

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
# Helper function to order arguments by name
#
orderby.name <- function (args) args[order(names(args))]

##
# Hash a function call
# 
hash.functionCall <- function (fc)

  # get functions default arguments
  formals(fc$f) %>% defaultArgs() %>%
  
  # add the unset default arguments to the argument list
  unset.defaultArgs(fc$args) %>% c(fc$args) %>%
  
  # order arguments by name
  orderby.name() %>%
  
  # force the values
  lapply(force) %>%
  
  # add the name of the function
  c(fc$name) %>%

  # hash the function call
  hash()
