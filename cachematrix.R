## Using makeCacheMatrix, set up the matrix. Then the name of 
## the matrix appended with $set() defines what is in the matrix
## using $get() is how to see the matrix. cacheSolve() solves (gives
## the inverse of) the matrix if it hasn't been done before,
## or retrieves it from the cache if it has. $set to set a new 
## matrix clears the cache for that matrix.

## Sets up a matrix to be used with this pair of functions
## such that the following function will return the inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##This function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
