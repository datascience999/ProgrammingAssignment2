## This file caches the inverse of a given matrix. 


## This function creates a special "matrix" object that can cache its inverse. 
## It returns a list of 4 functions. 
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    # Set matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get matrix
    get <- function() x
    
    # Set inverse of matrix
    set_inv <- function(solve) m <<- solve
    
    # Get inverse of matrix
    get_inv <- function() m
    
    # Return a list with length = 4
    list(set = set, get = get,
        set_inv = set_inv,
        get_inv = get_inv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
    ## Try to get matrix inverse from cache
    inv <- x$get_inv()
    
    ## If inverse is already in cache, return it without further calculations
    if(!is.null(inv)) {
        return(inv)
    }
    else {
        ## If inverse is not in cache yet, calculate it and save it into cache
        orig <- x$get()
        inv <- solve(orig, ...)
        x$set_inv(inv)
        inv
    }

}


