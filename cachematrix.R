## These functions take a matrix and cache it. 
## They also a compute its inverse and cache it.

## This function sets the value of the matrix, and returns a list that contains 
## functions to: set the value of the matrix, get the value of the matrix, 
## set the value of the matrix inverse, and get the value of the matrix
## inverse. It caches Both the matrix and the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    
}


## This function returns a cached matrix inverse. If there is no cached matrix
## inverse, it uses the solve function to compute it. It then caches it
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}