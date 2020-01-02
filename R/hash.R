# Hash function
hash <- function(value) digest::digest(value)

# Helper to create hash from function and args details.
hashFunctionCall <- function(name, formals, args) {
  
  # get default args
  args.default <- formals[
    unlist(
      lapply(formals,
             function(arg) {arg != ""}))]
  
  # get unset default args
  args.set <- c(
    args,
    args.default[
      unlist(
        lapply(names(args.default),
               function(name) {!name %in% names(args)}))])
  
  print(args.set)
  
  # get args hash
  name %>%
    c(unlist(args.set, use.names = FALSE)) %>%
    hash()
}