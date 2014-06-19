# This .R file contains the code that improves the efficiency of matrix inversion 
# by caching the inverse of a matrix rather than compute it repeatly.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) inver <<- inversion
    getinversion <- function() inver
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversion <- x$getinversion()
    if(!is.null(inversion)) {
        message("getting cached data")
        return(inversion)
    }
    data <- x$get()
    inversion <- solve(data)
    x$setinversion(inversion)
    inversion
}

