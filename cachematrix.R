## R Programming assignment coursera week 3
## by gituser chintan-dshel

## below are the the two functions, first function store the inverses of 
## the matrixes solved by the second function and the second function checks
## the first function everytime before solving the inverse of the matrix
## if the inverse exists, second function caches the matrix from the first 
## function and skips the calculations.

## this function makeCacheMatrix creates the special matrix object that can store
## the inverse of the matrix. x is then set to y and m is emptied. all the matrix
## and inverse are saved as list so the the values can be called by $ operator.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## below function cacheSolve checks in the list in above function and gets
## the inverse value, if that matrix is not NULL, then the matrix is used and
## the calculation in skipped, otherwise the matrix is solved and values are cached
## back in the list.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

