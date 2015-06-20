## Makes a cache of the inverse of a matrix, and returns the cache 
##  if available. If not, it computes the inverse matrix

## Sets the cache of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y 
                i <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)        

}


## Takes a matrix input and returns the cached inverse, or computes the inverse
## if the cache is unavailable

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
