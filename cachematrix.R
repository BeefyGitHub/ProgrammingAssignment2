## Functions to cache and retrive the inverse of a square matrix. 
## Assumption: the input matrix is not singular.


## Creates a special matrix object that can cache/retrieve its inverse
## x$setinv: caches a matrix as the inverse of x
## x$getinv: returns cached inverse of x if available, or NULL ootherwise
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of matrix x. If the inverse was computed before, the
## cached version is returned. Otherwise, computes the inverse, caches the
## result and then returns it.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

