## makeCacheMatrix makes a matrix with extra goodies so cacheSolve can
## look up the inverse, and if found, print it, or otherwise solve it

## this makes a matrix and sets up some variables that are able to be located in
## the parent environment

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

## This looks for the inverse matrix in the parent environment and
## if it is found, print it, if not it will solve it and print

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      return(inv)
}
