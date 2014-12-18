## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Retrieving cached inverse")
                return (inverse)
        }
        inverse <- solve(x$get(), ...)
        x$setinverse(inverse)
        inverse
}

