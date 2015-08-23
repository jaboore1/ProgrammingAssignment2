## This function will save computation time by caching the inverse of a matrix
## A two-step process will first define the matrix and then calcuate the inverse
## of a matrix given in step 1

## For a given matrix with parameters defined in a variable x, those parameters are
## stored in a global variable and is available to other functions outside of the
## makeCahceMatrix function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        seti <- function(inv) i <<- inv
        geti <- function() i
        list(set = set, get = get,
            seti = seti,
            geti = geti) 
}


## For the global matrix defined in the makeCahceMatrix function, the inverse of
## that matrix is returned from cache rather than calculated on the fly.  If no
## such matrix exists, then the inverse of a matrix x is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$geti()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$seti(i)
        i
}
