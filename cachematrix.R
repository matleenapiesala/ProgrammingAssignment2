## Very interesting lexical scoping exercise. I postponed doing
## this for a week as it seemed daunting. Had I known it was 
## quite simple thing to do as long as I followed the example given,
## I would have done it much earlier!!


## Create a cache of the inversed matrix and return a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # create a function called "set"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # create a function called "get" that returns x
  get <- function() x
  # create a function that makes an inverse of m
  setinverse <- function(solve) m <<- solve
  # create a function that returns the inverse of m
  getinverse <- function() m
  
  # makeCacheMatrix returns a list of four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks if the inverse exists.
## Exists -> print message and inversed matrix
## Does not exist -> get the matrix x, solve the inverse and return it.

cacheSolve <- function(x, ...) {
  # get the inversed matrix
  m <- x$getinverse()
  # if it exists, print message and return inversed matrix.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if it does not exsist, get the matrix and solve the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
