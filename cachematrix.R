## Here we have two functions: makeCacheMatrix and cacheSolve.
## The first one stores a list of functions to create a matrix and the second one returns the inverse of this matrix.

## This function makes an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
        x <<- y
        s <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}

## This function calculates the inverse of the matrix got by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached solve result")
            return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}