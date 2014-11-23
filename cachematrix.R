## Assumption as given in the question -> matrix supplied is always inversible

## The function makeCacheMatrix creates a special function matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x ## returns x
  setInverse <- function(solve) m <<-solve ## inverse matrix is cached
  getInverse <- function()m ## m is returned
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Cache is called for if inversed value is already available    
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Inverse is calculated since it is not available in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  
  ## Inversed matrix is returned
  m
}
