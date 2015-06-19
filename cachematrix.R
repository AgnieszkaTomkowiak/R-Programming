## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  set <- function(y) {
    x <<- y;
    # since the matrix is assigned a new value ("y"), flush the cache
    cache <<- NULL;
  }
  
  # returns the stored matrix
  get <- function() x;
  
  # cache the given argument 
  setInverse <- function(Inverse) cache <<- Inverse;
  
  # get the cached value
  getInverse <- function() cache;
  
  # return a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  # get the cached value
  cache <- x$getInverse()
  
  # if a cached value exists return it
  if(!is.null(cache)) {
    message("Getting cached data...")
    return(cache)
  }
  
  # otherwise get the matrix, calculate the inverse and store it in the cache
  data <- x$get()
  cache <- solve(data, ...)
  x$setInverse(cache)
  
  # return the cache
  cache
}
