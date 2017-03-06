## cachematrix allows for matrix inverse calculations to cached in order
## eliminate need to redo calculation everytime inverse is needed.

## Creates a special "matrix", which is a list containing a function to
## set the value of matrix
## get the value of matix
## set the value of inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Calculates the inverse of the special "matrix", however it first checks
## to see if inverse has already been calculated. If so, it gets inverse from 
## cache and skips calculation.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
