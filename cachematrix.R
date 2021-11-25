makeCacheMatrix <- function(x = matrix(0)) {
  # The function makeCacheMatrix() returns un object containing the four
  # functions set(), get(), setinverse() and getinverse() and two data objects
  # inv and x which are matrices.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Returns a matrix 'inv' that is the inverse of the matrix contained in the
  # object 'x' of the type makeCacheMatrix.
  
  # Retrieve potentially cached value of 'inv'. Will be NULL if the inverse
  # has not been calculated previously
  inv <- x$getinverse()
  # Check if the inverse was already calculated i.e. 'inv' is not NULL
  # If it was, return the cached value and don't calculate the inverse again
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse was not calculated previously, calculate it and cache it
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  message("solving")
  inv
}
