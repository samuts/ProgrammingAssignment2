## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Get the value of the matrix
    get <- function() x

    # Set the value of the inverse of the matrix
    setsolve <- function(solve) m <<- solve

    # Get the value of the inverse of the matrix
    getsolve <- function() m

    # return
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # retrieve from cache
    m <- x$getsolve() 

    # If is not null matrix, print message and return
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # If cache isn't filled yet

    # get it
    data <- x$get()  

    # solve it     
    m <- solve(data, ...) 

    # set it
    x$setsolve(m)	 

    # return     
    m		      		
}
