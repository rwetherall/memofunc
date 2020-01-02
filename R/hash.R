# Hash function
hash <- function(value) digest::digest(value)

# Helper to create hash from function and args details.
hashFunctionCall <- function(name, formals, args) {
  
  # get default args
  args.default <- defaultArgs(formals)
  
  # get unset default args
  args.set <- c(
    args,
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args)}))])
  
  # get args hash
  name %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
}


defaultArgs <- function(formals) {
  
  #formals <- formals(f) # TODO pass in function directly
  
  # return the arguments that have defaults
  formals[
    sapply(
      formals, 
      function(arg) {
        tryCatch( { 
          
          # try and evaluate the argument
          eval(arg)
          TRUE 
          }, 
          
          # an error indicates that the argument doesn't have a default 
          error = function (err) return(FALSE))
      })]
}

hashFnCall <- function (f, call) {

  # TODO need to provide the default arguments (or formals?)
  # TODO what happends if the default value is ""??  Need a better way to do this!!
  
  # get default arguments
  args.default <- defaultArgs(formals(f))
  
  # TODO need to force the evaluation of the arguments using force()
  args <- call[[-1]]
  
  # add values of unset default values to arg list
  args.set <- c(
    args,
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args)}))])
  
  # get args hash
  call[[1]] %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
  
}