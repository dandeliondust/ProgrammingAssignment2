## The following functions cache the inverse of a matrix.
## These will drastically reduce the time required to compute matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## m is set to NULL when the function is called for the first time.
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
            ## Assigning x the value of y. Search the parent environments to
            ## find the x that was passed to the function makeCacheMatrix()
            ## when it is called for this variable.
            ## When a new matrix is set, x and m are reset to y and NULL
            ## respectively.
            x <<- y
            m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the solution of the matrix
        setsolve <- function(solve) m <<- solve
        ## get the value of the solution of the matrix
        getsolve <- function() m
        ## The makeCacheMatrix contains copies of matrix - set and get
        ## and vectors - setsolve and getsolve.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inversee has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## get m from x, which is the object returned from makeCacheMatrix
    m <- x$getsolve()
    ## if m is not null (meaning m has been calculated before with current data)
    if(!is.null(m)) {
        ## a message will appear
        message("getting cached inverse")
        ## and the m value calculated before will be returned
        return(m)
    }
    ## if m is null (meaning m has not been calculated before or new data
    ## is assigned to the object)
    data <- x$get()
    ## retrieve the new data, and solve the matrix
    m <- solve(data, ...)
    ## save the solution to x
    x$setsolve(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
