## caching the inverse of a matrix 
## assumption: x invertible matrix

## makeCacheMatrix function taking x as invertible matrix and catch the x inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set matrix function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get matrix function
  get <- function() x
  
  ## set inverse matrix and get inverse matrix function
  setinverse <- function(inverse) m <<- solve(inverse)
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve return the inverse matrix after computed by function makeCacheMatrix.
## Condition: with same matrix then retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if available in cache then retrieve and return from cache
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## not available in cache then inverse matrix and cache inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
}
