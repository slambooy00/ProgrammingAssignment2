#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it 
#repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse01 <- NULL
  set <- function(y) {
    x <<- y
    inverse01 <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse01 <<- inverse
  getInverse <- function() inverse01
  list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse01 <- x$getInverse()
  if(!is.null(inverse01)) {
    message("getting cached data")
    return(inverse01)
  }
  mat <- x$get()
  inverse01 <- solve(mat)
  x$setInverse(inverse01)
  inverse01
}
