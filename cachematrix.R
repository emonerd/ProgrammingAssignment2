## Creating matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
#z is inverse
    set <- function(y) {
    x <<- y
    z <<- NULL
  }
get <- function() x
setInverse <- function(inverse) z <<- inverse
getInverse <- function() z
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## Computing the inverse of the special matrix
## In case already computed, retrieving from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getInverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  m <- x$get()
  z <- solve(m, ...)
  x$setInverse(z)
  z
}
