## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

      invert <- NULL
      set <- function(y) {
            x <<- y
            invert <<- NULL
      }
      get <- function() x
      setinvert <- function(inverse) invert <<- inverse
      getinvert <- function() invert
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invert <- x$getinvert()
      if(!is.null(invert)) {
            message("getting cached data")
            return(invert)
      }
      data <- x$get()
      invert <- solve(data, ...)
      x$setinvert(invert)
      invert
}

