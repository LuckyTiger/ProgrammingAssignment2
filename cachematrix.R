## @author Li Longyuan
## these two functions implements a way to get a matrix's inverse
## avoid of repeated computations.

## the function makeCacheMatrix() make a matrix into a specific matrix which contain
## a list of funtions: set(), get(), setinverse(), get inverse()
makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y){
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) r<<- inverse
        getinverse <- function() r
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the function cacheSolve() does return the inverse of a CacheMatrix  
## whether run solve(), or read cache data, depends on whether the current 
## matrix inverse exists.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getinverse()
        if(!is.null(r)){
                message("getting cached inverse matrix")
                return (r)
        }
        m <- x$get()
        r <- solve(m,...)
        x$setinverse(r)
        r
        
}
