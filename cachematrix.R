## This script file allows us to compute the inverse of a matrix.  After it has been
## computed the value is cached so that it can be looked up quickly in the future.


## The makeCacheMatrix function accepts a matrix and creates a special "matrix" that will
## hold a cached inverse matrix if it has already been computed.  The special matric is
## a list that holds helper functions for caching purposes.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    # Getter for the special matrix.
    get <- function() {
        x
    }
    
    # Setter for the special matrix.
    set <- function(value) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Getter for the inverse of the matrix, NULL by default and populated when cached.
    getInverseMatrix <- function() {
        inverseMatrix
    }
    
    # Setter for the inverse of the matrix.
    setInverseMatrix <- function(value) {
        inverseMatrix <<- value
    }
    
    # Return the special matrix as a list.
    list(get = get, set = set, getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix)
}


## The cacheSolve function takes in a special matrix (as produced from the makeCacheMatrix 
## function) and returns the inverse of a matrix.  If the inverse has already been cached
## it will used the cached value.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    
    ## If a cache hit is found, returned the cached value.
    if (!is.null(inverseMatrix)) {
        message("Getting cached inverse matrix")
        return (inverseMatrix)
    }
    
    ## Get the matrix.
    data <- x$get()
    
    # Use the solve method to compute the inverse matrix for us and cache it in our special object.
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    
    # Return our inverse matrix.
    return (inverseMatrix)
}
