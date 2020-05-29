## Put comments here that give an overall description of what your
## functions do

## makeChaceMatrix: stores the original matrix and provides handling functions
## x: original matrix - it is a copy of x only

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: gives the inverse of matrix 'x'. 
## If it has not been counted yet, calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getinverse())) {
        m <- x$get()
        x$setinverse(solve(m, ...))
    } 
    
    x$getinverse()
}
