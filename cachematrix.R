## When used in conjunction, the two functions below will
## return the cached value of the inverse of a matrix if it
## has previously been calculated. Otherwise, it will calculate
## the inverse and cache the result for future use.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        setM <- function(y) {
                x <<- y
                invM <<- NULL
        }
        getM <- function() x
        setInv <- function(inverse) invM <<- inverse
        getInv <- function() invM
        list(setM = setM, getM = getM,
             setInv = setInv,
             getInv = getInv)
}
 
 
## cacheSolve will attempt to return the cached inversed matrix
## if it exists. Otherwise, it will calculate the inverse using
## the solve() function in R and will cache the result.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$getM()
        invM <- solve(data, ...)
        x$setInv(invM)
        invM
}
