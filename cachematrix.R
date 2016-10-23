## These functions perform 2 functions.  
## 
## makeCacheMatrix copies a matrix into memory so it doesn't have to be read
## in evertime by other functions or programs
##  
## cacheSolve creates an inverse matrix from a matrix that is passed to it

## places a matrix into system cache

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    get <- function() x
    setInverse <- function(solveM) inverseM <<- solveM
    getInverse <- function() inverseM
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

## creates the inverse of a matrix that is passed to it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (is.null(inverse)) {
            message("retrieve cached data")
            return(inverse)
        }
        cachedata <- x$get()
        inverse <- solve(cachedata)
        x$setInverse(inverse)
        inverse
}
