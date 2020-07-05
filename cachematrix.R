## In this assignment, I wrote a pair of functions that cache the inverse of a matrix.
## Here, we are assuming that the matrix supplied is always invertible.

## The function  makeCacheMatrix creates a special matrix like object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the matrix like objetic 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated and the matrix has not changed, then cacheSolve function should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
