## This pair of functions provides a way to cache matrix inverse results.
## It does so by leveraging the lexical scoping rules.

## The makeCacheMatrix functions creates the actual cache. It creates a list with functions to
## get and set the matrix, as well as functions getsolve (to obtain the inverse) and setsolve
## (to actually calculate it if needed)
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


## The cacheSolve funciton is the wrapper function that provides access to the modified solve functionality.
## It does so by checking if a cache already exists (via the getsolve function) and returns that if it 
## exists. Otherwise, it calls the typical solve method,caches it, then returns the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
