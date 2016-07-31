## Function for using cache to speed up calculation of inverse matrix

## Special object to store matrix with cache inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinvers <- function(i) inverse <<- i
    getinvers <- function() inverse
    list(set=set, get=get, setinvers=setinvers, getinvers=getinvers)
}


## Function for calculating inverse matrix for x with using cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvers()
    if (!is.null(inv)) {
        message('Getting inverse from cache')
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinvers(inv)
    inv
}
