## This is a set of functions designed to hold a square matrix
## and store the inverse of that matrix after its computation.

## makeCacheMatrix takes a matrix variable and holds it in a
## structure prepared to also contain its inverse. The matrix
## may be defined either in the initial call, or by using the
## $set subfunction.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve retrives the inverse of a makeCacheMatrix either
## by returning a previously computed inverse, or by computing
## the inverse if one had not already been created. 

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}