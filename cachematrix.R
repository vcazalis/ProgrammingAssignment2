## This pair of functions are meant to cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) m <<- inv
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix".
## If the inverse has already been calculated, it will retrieved the stored value.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     ## Return a matrix that is the inverse of 'x'
     m
}
