## The set of functions to calculate the inverse matrix with possibility to cache the result for
## the reusing e.g. in loops

## The function makeCacheMatrix makes "special" list of functions (function factory) to set the value of matrix,
## to get the matrix, to set the value of inverse matrix and to get the inverse matrix 
##(practically the same as in an example)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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




## The function cacheSolve returns the inverse matrix prepared with makeCacheMatrix(). 
## In the case of the reuse returns the previously cached result with message: "getting cached data" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
