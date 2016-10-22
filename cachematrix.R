# These functions create a matrix that can cache its inverse

# This function creates the CacheMatrix from an ordinary matrix x. A CacheMatrix object has
# the following functions:
#   set(m): set the value of the matrix to m (clears the cache)
#   get():  returns the matrix
#   set_inverse(inv): set the cached inverse of ths matrix to inv
#   get_inverse(): returns the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# This function returns the inverse of a CacheMatrix x. If x has a cached inverse, this function
# will return it. Otherwise, the function will compute the inverse of x, store it in x's cache,
# and return the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ret <- x$get_inverse()
    if (!is.null(ret)) {
        message("getting cached data")
        return(ret)
    }
    mat <- x$get()
    ret <- solve(mat)
    x$set_inverse(ret)
    ret
}

