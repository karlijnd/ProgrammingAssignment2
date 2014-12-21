## These functions cache the inverse of a matrix
## When a requested matrix is already cached, it saves time and computation power

## create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the inverted matrix
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ## get the values of the matrix
    get <- function() x
    ## invert the matrix
    setsolve <- function(solve) s <<- solve
    ## get the inverted matrix
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## nog niet aangepast
    ## calculates the mean of the special "vector" created with the above function.
     
    s <- x$getsolve()
    ## checks to see if the mean has already been calculated.
    if(!is.null(s)) {
        message("getting cached data")
        ## If so, it gets the mean from the cache and skips the computation.
        return(s)
    }
    data <- x$get()
    ## Otherwise, it calculates the mean of the data
    s <- solve(data, ...)
    ## and sets the value of the mean in the cache via the setmean function.
    x$setsolve(s)
    s
    
    ## Return a matrix that is the inverse of 'x'
}