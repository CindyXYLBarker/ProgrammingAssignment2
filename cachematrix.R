## Utilities that provides caching support 
## for Matrix inversion function

## Function that creates a special matrix object
## that has cached inversion
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) i <<- solve
  getSolve <- function() i
  list(set=set, get=get, setSolve=setSolve, 
       getSolve=getSolve)
}

## Function that implements caching support for
## matrix inversion
cacheSolve <- function(x, ...) {
  i <- x$getSolve()
  if (!is.null(i)){
    message("getting cached version of inversion")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
}
