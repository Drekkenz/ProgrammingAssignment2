## These functions combined create a list of 
## functions that describe a matrix and cache it's inverse.

## This function creates the list of functions, and stores 
## the inverse(inv) variable as NULL when called.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(w) {
    x <<- w
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function determines the inverse of the matrix in the 
## above function, returns it, then saves the inv result in the 
## above environment. Given that it is not already cached. 
## If cached, it simply returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Calculates inverse of the matrix
  inv <- solve(data, ...)
  ## Stores the inverse
  x$setinverse(inv)
  inv
}