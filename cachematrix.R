## makeCacheMatrix takes in a single matrix argument.
## x must be an invertible matrix.
## It creates a matrix object and can store the inverse of matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set() assigns new matrix y to replace x in parent environment.
        ## This indicates that the inverse matrix 'inv' of x which exists in the cache
        ## must be deleted. Hence it sets inv to NULL in parent environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get() returns the matrix x
        get <- function() x
        ## setinv() sets inv to the inverse of matrix x
        setinv <- function(solve) inv <<- solve
        ## getinv() returns inv
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes in the matrix object from makeCacheMatrix. It checks if
## inverse has already been calculated. If it exists, it returns inv from cache.
## If no cached inv, it calculates inv and sets it in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ## check for inverse. If exists, returns cached inv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
