## Following functions handles a cachable matrix operations

## Function to create special matrix object that is able to, set value, 
## get value, set inverse & get inverse
## Input: Square matrix
## Output: List of functions to set, get, set inverse & get inverse

makeCacheMatrix <- function(mat = matrix()) {
    ## Object to store cached matrix inverse
    mat_inverse <- NULL
    
    ## Set matrix with new value, and resets cached matrix inverse
    set <- function(y) {
        mat <<- y
        mat_inverse <<- NULL
    }
    
    get <- function() mat
    
    setinverse <- function(inverse) mat_inverse <<- inverse
    
    getinverse <- function() mat_inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns cached matrix inverse if exists, or re-computes new inverse

cacheSolve <- function(x, ...) {
    ## Get cached matrix inverse
    m <- x$getinverse()
    
    ## If matrix inverse was computed before, return cached value, otherwise
    ## compute new inverse
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    ## Re-compute matrix inverse and store value in matrix inverse cache object
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
