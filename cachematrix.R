## R Programming, Programming Assignment 2
## See Unit Test Cases at
## https://class.coursera.org/rprog-008/forum/thread?thread_id=174

makeCacheMatrix <- function(x = matrix()) {
# This function creates a special "matrix" object that can cache its inverse.
# Assumes that the matrix supplied is always invertible.
    
    # Local variables to store m & m inverse
    m <- x
    minverse <- NULL
    
    # Return m
    get <- function() m
    
    # Return cached value for m inverse
    getinverse <- function(x) minverse
    
    # Change m if, and only if, its value has changed
    # if m changes, m inverse is not valid anymore
    # so we must clear it
    set <- function(x) {
        if(!identical(x, m)) {
            m <<- x
            minverse <<- NULL
        }
    }
    
    # Set cached value for m inverse
    # only if not calculated
    setinverse <- function() {
        if(is.null(minverse)) minverse <<- solve(m)
    }
    
    # Return value
    list (get = get,
          getinverse = getinverse,
          set = set,
          setinverse = setinverse)
}

cacheSolve <- function(x, ...) {
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not
# changed), then the cacheSolve should retrieve the inverse from the
# cache.

    # If, and only if, m inverse is NULL
    # (i.e., m inverse has not been calculated yet),
    # then compute & cache m inverse
    if(is.null(x$getinverse()))
        x$setinverse()
    else
        message("Getting cached data for matrix inverse")

    # Finally, return cached value for m inverse
    x$getinverse()
}
