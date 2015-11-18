## These functions implement functionality to cache a matrix inverse
## calculation. It does this by wrapping the argument matrix in a
## list of functions that expose functionality to
##   get - retrieve the wrapped matrix
##   set - set the wrapped matrix
##   getInverse - get the inverse value
##   setInverse - set the inverse value

## Returns a list of functions that can act as a wrapper around the argument
## matrix to cache the matrix inverse calculation.
makeCacheMatrix <- function(mat = matrix()) {
	cachedInverse <- NULL
        set <- function(newMatrix) {
                mat <<- newMatrix
                cachedInverse <<- NULL
        }
        get <- function() mat
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x'. If there was no cached inverse
## value, computes the inverse and caches it. The argument should be a
## container consistent with the behavior of the returned list from
## makeCacheMatrix.
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
