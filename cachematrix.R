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

cacheSolve <- function(x,...) {
        
        m <- x$getInv()
        if(is.null(m)==F) { message("getting cached data")
                return(m) }
        m <- solve(x$get(), ...)
        x$setInv(m)
        m
}
