## Calculating and caching the inverse of a matrix.

## Creates a custom matrix object that holds the cache of its inversion

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInversion <- function(inversion) inv <<- inversion
        getInversion <- function() inv
        list(set = set, get = get,
             setInversion = setInversion,
             getInversion = getInversion)
}


## Calculates and caches a matrix inversion. Returns the cached value if exists

cacheSolve <- function(x, ...) {
        inv <- x$getInversion()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInversion(inv)
        inv
}
