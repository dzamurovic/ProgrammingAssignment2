## This function does inversion of the given matrix
## and create a matrix that is able to cache its inverse.

## Function makeCacheMatrix is given a matrix as a parameter and returns an list as a result.
## List contains four functions, two "getters" of the two things we are storing, 
## and two "setter" functions to change them.

makeCacheMatrix <- function(x = matrix()) {
        # m will be inverse matrix
        m <- NULL

        # set the original matrix and (re)init its inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # return the original matrix
        get <- function() x
        
        # this is called during the first cacheSolve() call
        setInverse <- function(inverted) m <<- inverted
        
        # return the cached inverted matrix
        getInverse <- function() m
        
        # return the list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Get the inverted value of matrix x (stored in x).
        m <- x$getInverse()
        
        # If the inverted value was cached (not NULL) ...
        if(!is.null(m)) {
            message("Returning cached data...")
            # ... return the inverse matrix and exit the function.
            return(m)
        }

        # If the inverted value was not cached...
        data <- x$get()
        
        # ... calculate it...
        m <- solve(data, ...)
        
        # ... store it in given x...
        x$setInverse(m)
        # ... and return it as result of the function.
        m
}
