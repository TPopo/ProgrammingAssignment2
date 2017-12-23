## the 1st function creates a special matrix that adds variables that are able to 
## be searched in the parent environment, as well as gives functions names so
## they can be accessed easily and/or used in cache solve

## this creates a matrix, nulls inv which essentially "clears the cached inverse"
## as well as defines setters and getters

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

## looks in the parent environment to see if the inverse is there and return it,
## otherwise it will solve for the inverse and return it

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