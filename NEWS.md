
# memofunc 1.0.3

* bug: storage methods are not redirected to the correct implementation when type is specified

# memofunc 1.0.2

* Collapsed cache and storage into one concept
* CRAN feedback

# memofunc 1.0.1

* CRAN feedback

# memofunc 1.0.0

* Added memo function, allowing functions to be memoized.
* Added simple in memory cache that can store values against keys generated from the parameter values of memoised functions.
* Added function allowing access to the original function from the memoised function.
* Added function that can test for a memoised function.
* Added access to a memoised functions cache.
* Added simple cache management functions including clearing and listing the items in the cache.
* Added a `NEWS.md` file to track changes to the package.
* Added support for NULL return values and methods with no return value with allow.null parameter
* Improved memo tests
* Move hash functions to separate file adding tests
