## Caching of a matrix
## Caching the inverse of a matrix is potentially time consuming
##if the contents of the vector are not changing it is beneficial to cache the value
## of the mean for quick look up when needed. 

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }

  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of the special matrix created above
## if the inverse has already been calculated it should retrieve the inverese 
## from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
      message("retrieving cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  }
