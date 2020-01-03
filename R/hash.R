# Helper to create hash from function and args details.
hashFunctionCall <- function(name, formals, args) {
  
  # get default args
  args.default <- defaultArgs(formals)
  
  # get the unset default args
  args.default.unset <- 
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args.force)}))]
  
  # evaluated set arguments
  args.set <- lapply(c(args, args.default.unset), force)
  
  # get args hash
  name %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
}

defaultArgs <- function(formals) {
  
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

# Hash function
hash <- function(value) digest::digest(value)