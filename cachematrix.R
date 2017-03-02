##  this function creates a matrix that makes it possible to cache the 
## solution to the inverse of a matrix.  It does so by creating a        
## list of functions that allow you to get, set the values depending ## on whether it's cached.

## when called w/invertible matrix argument it creates a list of     
## functions that can be accessed by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## this func does the "work" of solving and storing the solved value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

