## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object with a set of methods

## get - mat$get() returns the matrix
## set - mat$set(matrix) sets the matrix (so you can change it at a later point)
## getinverse - mat$getinverse() gets the inverse (will be null if not set, use cacheSolve to set it)
## setinverse - mat$setinverse(inverse matrix) sets the inverse, not really useful from the command line. But does
## allow you to set the inverse if you want. Really for use by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
# insures inverse is set to dull when the matrix is reset
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## For matrix objects created with makeCacheMatrix, this function return the value of the inverse if
## it has been cached or calculates the value  of the inverse if it hasn't

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
# Check to see if the inverse has been set
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
