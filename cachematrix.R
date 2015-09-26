## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversion
## get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) m <<- solve
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## The following function calculates the inversion of the special "vector" created with the above function.
## It first checks to see if the inversion has already been calculated. If so, it gets the inversion from the cache and skips the computation. 
## Otherwise, it calculates the inversion of the data and sets the value of the inversion in the cache via the setinversion function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinversion()
    if(!is.null(m)) {
      message("getting cached inversion")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    m
}
