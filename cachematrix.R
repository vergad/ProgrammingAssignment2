## These two functions allow caching the calculation of the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its
## inverse. It returns a list of four functions to set and get the value of
## a matrix and its inverse. The function argument initializes the matrix x.
## When setting the matrix to a new value, the cached inverse, invX, is set
## to NULL.
makeCacheMatrix <- function(x = matrix()) {
   invX <- NULL
   set <- function(y) {
       x <<- y
       invX <<- NULL
   }
   get <- function() x

   setInverse <- function(inv) invX <<- inv
   getInverse <- function() invX
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The second function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves it from the cache.
## Otherwise, it calculates it using the solve() method. In both cases, the
## function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if(!is.null(invX)) {
        message("retrieving cached inverse")
        return(invX)
    }
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setInverse(inv)
    inv
}
