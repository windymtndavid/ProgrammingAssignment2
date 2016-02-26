## makeCacheMatrix/cacheSolve are a set of functions designed to return the inverse
## of a given matrix

## makeCacheMatrix is a list of functions that are used to set/get a matrix and
## then set/get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved_inv) inv <<- solved_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve retreives cached matrix inverse
## if matrix inverse is not cached, it creates a new one
## if matrix inverse is cached, it returns the cached inversw

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## esle return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}