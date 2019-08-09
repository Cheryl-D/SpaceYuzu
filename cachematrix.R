## main purpose: cache the inverse of a matrix
## makeCacheMatrix creates a matrix that contains a function for 4 purposes: .
#1. Set the values of the matrix
#2. Get the values of the matrix
#3.Set the value of the inverse
#4.Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  cacheInvMatrix <- NULL

  set <- function(y) {

    x <<- y

    cacheInvMatrix <<- NULL

  }

  get <- function() x

  setInverse <- function(inverse) cacheInvMatrix <<- inverse

  getInverse <- function() cacheInvMatrix

  list(set = set, get = get,

       setInverse = setInverse,

       getInverse = getInverse)

}

## This function  calculates the inverse of the special matrix returned by the function makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
    
        m <- x$getInv()
        if(is.null(m)==F) { message("getting cached data")
                                 return(m) }
        m <- solve(x$get(), ...)
        x$setInv(m)
        m
}
