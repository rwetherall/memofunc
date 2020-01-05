require(digest)

##
#
#
unused.formals <- function (formals, args) {
  
  names(formals[
    unlist(lapply(
      names(formals),
      function (name) {!name %in% names(args)}))])
}

##
#
#
name.args <- function (formals, args) {
  
  unused.formals <- unused.formals(formals, args)
  names.args <- names(args)
  
  if (is.null(names.args)) {
    unused.formals[c(1:length(args))]
  } else {
    
    index <- 1
    unlist(lapply(names.args, function(name) {
      
      if (name == "") {
        result <- unused.formals[[1]]
        index <<- index + 1
        result
      } else {
        name
      }
    }))
  }
}

##
#
#
functionCall <- function (f = sys.function(sys.parent()), call = sys.call(sys.parent())) {
  
  name <- call[[1]]
  args <- as.list(call)
  args[[1]] <- NULL
  
  # make sure all the passed args are named
  args.all.names <- name.args(formals(f), args)
  names(args) <- args.all.names
  
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
unset.defaultArgs <- function (defaultArgs, args)
  defaultArgs[
    unlist(lapply(
        names(defaultArgs),
        function (name) {!name %in% names(args)}))]

##
# Hash a function call
# 
hash.functionCall <- function (fc) {
  
  all.args <- 
    formals(fc$f) %>%
    defaultArgs() %>% 
    unset.defaultArgs(fc$args) %>% 
    c(fc$args)
  
  all.args <- all.args[order(names(all.args))]
  
  all.args.forced <- unlist(all.args, use.names = FALSE) %>% lapply(force)
  
  hash(c(all.args.forced, fc$name))
}
