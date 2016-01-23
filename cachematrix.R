## These functions paired together store an invertible matrix, calculate the inverse and store in cache for retrieval

## makeCacheMatrix will read in a square matrix and store the inverse to cache

makeCacheMatrix <- function(x = matrix()) {
##make M NULL when the function is called
        m <- NULL
##setting x to the passed in matrix & m to Null
## using<-- to make availabe outside of local environment
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
## returns passed in matrix    
        get <- function() x
## setsolve is a function sets the value of
## the variable m to the result of the inverse of the inbound matrix
## i.e. solve(x)
        setsolve <- function(solve) m <<- solve
##returns the value of m
        getsolve <- function() m
## stores the functions in a list     
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
    

}
## CacheSolve will read in the matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
## set m to the value of the cached matrix inverse
## check to see if there is a value
## if there is a non null value - note that it is cached information
## return the value
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
## if not value is present
## grab the matrix that was passed into makeCacheMatrix
## calculate the inverse and return the value
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  
    
}
