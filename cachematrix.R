## Use of the <<- operator in the implementation of two functions designed to
## cache the inverse of a matrix, so as to, avoid repeatedly computing the
## inverse of the same matrix.

## This first function (makeCacheMatrix) creates a special "matrix" object
## that can cache its inverse. It takes a matrix as its argument.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The second function (cacheSolve) computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been created (and the matrix has not changed), then the cachesolve
## retrieves and returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
