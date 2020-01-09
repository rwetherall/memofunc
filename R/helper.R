require(magrittr)

##
# Helper function to get the first n of a list
#
first.n <- function (x, n=1) x[c(1:n)]
`%<n>%` <- function (x, n=1) first.n(x,n)

##
# Helper function to pad out a list to size n with a specified value defaulting to NA
#
pad <- function (x, n, by=NA) lapply(1:n, function (index) if (index > length(x)) x[[index]] <- by else x[[index]])

##
# Helper function to order arguments by name
#
orderby.name <- function (args) args[order(names(args))]

##
# Helper function to remove an item from a list
#
removeby.name <- function (x, name) if (name %in% names(x)) x[sapply(names(x), `!=`, y = name)] else x

##
# Helper to get the rest of a given list
#
rest <- function (x) if (length(x) <= 1) list() else x[2:length(x)]

##
# Helper to insert expression into function to be executed before the current body.
#
insert.before <- function (f, expr) {
  
  expr.rest <- function (expr) {
    
    expr.list <- expr %>% as.list()
    
    if (length(expr.list) == 1) expr.list else rest(expr.list)
  }
  
  body(f) <- c(`{`, expr.rest(expr), expr.rest(body(f))) %>% as.call()
  
  f
}