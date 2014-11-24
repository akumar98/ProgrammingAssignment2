## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This program contains two functions
##      makeCacheMatrix: which creates a special matrix object able to cache its
##                      inverse
##      cacheSolve which returns the cached inverse of a special matrix if it has
##                  been previously calculated and the matrix has not changed, or
##                  calculates, caches and returns the inverse of the matrix if not

## Function : makeCacheMatrix
## Parameters: x data contents (matrix) of the special matrix object
## Short description:   this function creates a special matrix object able to
##                      cache its inverse
## Detailed Description:    this function creates an object (in the OO sense of
##                          the word), which includes a matrix (the data) and
##                          several methods for managing this data (set and get)
##                          and for managing and caching the inverse of the
##                          matrix (setinv, getinv). All these methods are packed
##                             in a list for them to be accessed.
## Returns: an special matrix object

makeCacheMatrix <- function(x = matrix()) {
    # Initializing inverse property
    i <- NULL
    
    # Set matrix method
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }
    # Get Method
    get <- function() x
    # Inverse set method
    setInverse <- function(inverse) i <<- inverse
    # Get Inverse matrix method
    getInverse <- function() i
    # List of methods
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function : cacheSolve
## Parameters: x data contents (matrix) of the special matrix object
## Short description:       This function returns the cached inverse of a special 
##                          matrix if it has been previously calculated and the matrix
##                          has not changed, or calculates, caches and returns the 
##                          inverse of the matrix if not
## Detailed Description:    This function gets the cached value of the special
##                          matrix object x, if this value is NULL it means that
##                          either the matrix has changed or its inverse has
##                          never been calculated. In this case the inverse of the
##                          matrix stored in the special matrix object is calculated
##                          using the solve function (the matrix must be invertible),
##                          the inverse is cached in the special matrix object and
##                          returned to the user. If not then the inverse has been calculated in the past
##                          and the function returns the cached value.
## Returns: the inverse of the matrix of the special matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
