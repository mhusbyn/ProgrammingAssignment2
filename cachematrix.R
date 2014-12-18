## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions to read and manipulate a matrix object (supplied as argument).
## This list can then be used in cacheSolve to get the inverse of the matrix object and cache the result in the list.
## The caching of the inverse makes it faster to retrieve on subsequent calls to the cacheSolve function.


## Write a short comment describing this function

## Returns a list that contains functions to read and manipulate the matrix supplied as the argument.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y # Give the matrix its new values
                inverse <<- NULL # Clear the cached inverse matrix
        }
        
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function

## Returns and sets the inverse matrix for the list created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) { # The inverse is cached so we can just return it without any computation.
                message("Retrieving cached inverse")
                return (inverse)
        }
        inverse <- solve(x$get(), ...) # Recompute the inverse
        x$setinverse(inverse) # And cache it
        inverse
}

