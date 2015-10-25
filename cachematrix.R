## This is a set of functions that creates an object 
## that stores a matrix and caches its inverse.

## the first function creates the function that is 
## able to source the cached Solve function. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y
  inv <<- NULL
  } 
  #this nested function retrieves cacheSolve
  get <- function() x
  setSolve <- function(solve) inv <<- solve
  getSolve <- function() inv
  list(set = set, get = get,setSolve = setSolve,getSolve = getSolve)
}

##this function calculates the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getSolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- Solve(data, ...)
  x$setSolve(inv)
  inv
  
}
