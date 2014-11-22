## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix creates a cacheable matrix, as described in the example.
##
## cacheSolve Inverts the matrix, caching the result.  It uses the 'solve' function.
## I consiedered using the ginv function but loading the MASS library seemed overhead.

## Write a short comment describing this function
## Creates a cacheable matrix, as described in the example

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		    x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Inverts the matrix, caching the result.  It uses the 'solve' function.
## I consiedered using the ginv function but loading the MASS library seemed overhead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
