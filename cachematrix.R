## makeCacheMatrix creates a matrix that can be cached

## cacheSolve computes the inverse of the matrix pointed by 
## the list of operations created by makeCacheMatrix and caches it
## for further calls to this function using same matrix

## This function takes a matrix as an input and returns
## a list of operations that point to the input matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function takes a list of operations as an input
## This list points to a matrix whose inverse needs to be computed
## If the inverse is already computed it will return from cache
## If the inverse is being computed for the first time, it will
## cache it after computation and then return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setInverse(i)
    i
}
