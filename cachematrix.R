## This file is the functions needed for inverting
## a matrix and caching that inversion as it is a
## costly operation


## makeCacheMatrix - Storage function for caching the
## results of the matrix inversion done by the
## cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix_val) m <<- matrix_val
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes the inverse of a passed matrix or the cached
## value if the matrix has been calculated already
cacheSolve <- function(x, ...) {
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
