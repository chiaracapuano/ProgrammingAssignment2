## The following functions compute and cache the inverse of a matrix.

## This function sets the value of the matrix, gets it, sets the value of the inverse and 
## gets it. The default matrix is a 5x5.

makeCacheMatrix <- function(x = matrix(, nrow = 5, ncol = 5)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function calculates the inverse of the matrix created in the step above. If the 
## value has already been computed, it gets the inverse of the matrix from the cache, 
## otherwise it computes it via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
