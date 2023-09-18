## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### The makeCacheMatrix function creates a cached matrix object.  The cached matrix object is a list object containing functions for caching the initial matrix input and its inversion.

makeCacheMatrix <- function(x = matrix()) {
        ### Creates a cached matrix object
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinversion <- function(inverted_matrix) m <<- inverted_matrix
        getinversion <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinversion = setinversion, getinversion = getinversion)
}


## Write a short comment describing this function
### The cachedSolve function takes a cached matrix object from makeCacheMatrix, checks if that matrix has been inverted yet, and if the matrix has not been inverted, it will calculate the inverted matrix and add it to the input cached matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ### Cached matrix object is required for input
        m <- x$getinversion()
        if(!is.null(m)) {
                message("Retrieving cached inverted matrix")
                return(m)
        }
        matrixcontents <- x$getmatrix()
        m <- solve(matrixcontents)
        return(m)
}
