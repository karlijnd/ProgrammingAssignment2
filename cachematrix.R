## These functions cache the inverse of a matrix
## When a requested matrix is already cached, it saves time and computation power

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    
    ## setter
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## getter
    get <- function() x
    
    ## Set inverted matrix.
    setsolve <- function(solve) s <<- solve
    
    ## Get the inverted matrix.
    getsolve <- function() s
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Checks to see if the inverse has already been calculated.
    s <- x$getsolve()

    ## If so, it gets the inverse from the cache and skips the computation.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    
    ## Otherwise, it calculates the inverse of the matrix...
    s <- solve(data, ...)
    
    ## and sets the value of the inverse in the cache via the setsolve function.
    x$setsolve(s)
    
    ## Return a matrix that is the inverse of 'x'.
    s
}