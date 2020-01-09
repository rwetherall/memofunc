require(digest)
require(magrittr)

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
# Get a 'functionCall', which is a hash'able class containing information about a call to a function
#
functionCall <- function (f = sys.function(sys.parent()), call = sys.call(sys.parent())) {
  
  # function call name
  name <- call[[1]]
  
  # function call arguments
  args <- call %>% as.list() %>% rest()
  
  # name all the function call arguments
  names(args) <- all.names(formals(f), args)
  
  # return functionCall
  list(f = f, name = name, args = args) %>% `class<-`("functionCall")
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
# Hash function, using body
hash.function <- function (f) hash.default(body(f))

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
# Hash a function call
# 
hash.functionCall <- function (fc) 
  
  # get functions default arguments
  formals(fc$f) %>% defaultArgs() %>%
  
  # add the unset default arguments to the argument list
  unset.defaultArgs(fc$args) %>% c(fc$args) %>%
  
  # order arguments by name
  orderby.name() %>%
  
  # force the values and hash, this ensures that things like functions are comparable in a consistant way
  lapply(force) %>% lapply(hash) %>%
  
  # add the hash of original function
  c(hash(fc$f)) %>%

  # hash the function call
  hash() 
