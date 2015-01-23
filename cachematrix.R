## a pair of functions that cache the inverse of a matrix
## one creates a matrix to cache its inverse, the other computes the inverse or retrieve it

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) im <<- solve
    getinverse <- function() im
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
##  if the inverse has already been calculated, retrieve the inverse from the cache

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinverse(im)
    im
}
