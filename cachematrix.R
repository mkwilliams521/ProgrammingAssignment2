## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix 
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invrs <<- solve
  getInverse <- function() invrs
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mtrx <- x$get()
  invrs <- solve(mtrx, ...)
  x$setInverse(invrs)
  invrs
}

mtrx1 <- matrix(c(1, 2, 3, 4), 2, 2)
solve(mtrx1)

