## R programming | Week 3 - Assignment 2
## This pair of functions aims to cache the Inverse of any invertible matrix 
## Lexical Scoping is used here in order not to compute several times the inverting operation

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" object or returns the cached value

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("Getting cached data!") # The cached value is returned if already computed
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m        
}
