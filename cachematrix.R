## Two functions below do the following
## makeCache Matrix creates a special 'Matrix' object that is a list object with functions
## that allow manipulations to this special Matrix. 
## cacheSolve solves this special Matrix object for an inverse only if there is no inverse cached in it.

## To test function input an invertible matrix to makeCacheMatrix and pass the return value
## to cacheSolve. Function cacheSolve can be used do calculate inverse for all future
## references

## makeCacheMatrix takes a matrix input and creates a list that contains four functions
## get and set are to be used to change the value of matrix in this special object
## setInv and getInv are to be used to set and get values of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() as.matrix(x)
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve works on the special Matrix object and returns its inverse from cache
## If inverse cannot be found in the cache it returns the same by calculating it and 
## sets the cache in the object

cacheSolve <- function(x, ...) {
    ## Input is a makeCacheMatrix object
    m <- x$getInv()
    
    ## Check for cache
    if(!is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    ## Calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    ## Set cache in the special Matrix object
    x$setInv(m)
    m
    
}
