## makeCacheMatrix provides a list of functions that act as an interface to
## a matrix and its inverse. cacheSolve take a list produced by the first
## function, and retrieves or calculates (and stores) the inverse, depending
## on whether or not it is already stored.

## These two functions are only slightly modified version of functions
## provided by rdpeng for the Coursera R Programming class.



## makeCacheMatrix
## The matrix x and its inverse i are stored in this function's environment.
## Setter and getter functions are provided for both the matrix and its
## inverse. Validity of the cached inverse is ensured because the matrix
## setter functions NULLs i.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse 
    
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
## This function provides a somewhat unnecessary layer to retrieve the 
## value of the inverse. If the value of the inverse is not already
## stored, then it calculates it and stores it.
## Probably a better solution would be to get rid of this
## function and put the useful bits straight into the getinverse
## function above.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()

    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
}
