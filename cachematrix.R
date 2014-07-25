### Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # If the inverse has already been calculated (and the matrix has not changed),
  # the inverse is retrieved from the cache and the computation is skipped
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise, the inverse of the matrix is calculated and the value of the inverse
  # is set in the cache via the setinverse function
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
