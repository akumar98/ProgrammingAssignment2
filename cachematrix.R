## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
