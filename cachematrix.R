## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a Matrix object that returns 4 functions:
# - set(), get(), getinverse(), setinverse()
#
# set()        - Sets the value of the matrix we want to cache
# get()        - Gets the current value of the matrix that is stored in 
#                the cache
# setinverse() - Function to set the value Inverse square value of a matrix in
#              - the cache
# getinverse() - Returns the current value of the Inverse square matrix  
#                in the cache. Defaults to NULL
#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# Function that returns the Inverse square value of a matrix.
# First, it tries to obtain the value from the cached of the passed object
# If the return value is NOT NULL, then it will return the cached value,
# else it will calculate the new value of the inverse squar


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
