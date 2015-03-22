## Matrix inversion is usually a costly computation
## In some ocasions is better to cache the inverse of a matrix, and reause the result,
## rather than compute it repeatedly. This pair of functions implements this approach of caching

## This function creates a list of basic functions to support the main function cacheSolve() 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function check in the parent envionment if exists a variable i,
## and if it is not NULL. If this variable exists it means that this matrix had already inverted,
## so rather than solve the matrix, the function uses the stored result

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    if (det(data) != 0) {
        i <- solve(data, ...)
        x$setinv(i)
        i
    } else {
        print("The matrix is not invertible.")
    }    
}
