## In this assignment, I wrote a pair of functions that cache the inverse of a matrix.
## Here, we are assuming that the matrix supplied is always invertible.

## The function  makeCacheMatrix creates a special matrix like object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve computes the inverse of the matrix like objetic 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated and the matrix has not changed, then cacheSolve function should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# I hope you enjoy this!
