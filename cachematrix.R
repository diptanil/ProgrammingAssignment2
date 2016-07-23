## The following code computes the inverse of matrix

## The first function, makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to
##  1. set the value of the matrix
##  2. the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mat_inv <<- solve
    getinv <- function() mat_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    mat_inv <- x$getinv()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data, ...)
    x$setinv(mat_inv)
    mat_inv
}
