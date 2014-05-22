## Programming Assignment 2
## Functions required to Get, Set a Matrix aswell as caching the 
##           the Matrix Inverse

## Create a Matrix and initialise functions to get and set and create
## the inverse

makeCacheMatrix <- function(x = matrix()) {   
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## Return a matrix that is the inverse of x, checking the 
## cache first

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
