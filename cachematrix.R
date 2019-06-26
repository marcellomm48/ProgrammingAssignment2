## These two functions combined return the inverse of a matrix. If a matrix
## has already been called by cacheSolve(), the returned inverse martix will 
## come from a cache rather than a new calculation, saving computing time.

## This function takes a matrix and creates a list of functions that
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix,
## and get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL
                set <- function(y) {
                        x <<- y
                        inverse <<- NULL
                }
                get <- function() x
                setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
                getinverse <- function() inverse
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## This function takes an object type makeCacheMatrix() and checks to see 
## if a cached inverse matrix exists. If so it returns that matrix. If not
## the function calculates the inverse matrix, stores that in the cache
## and then returns the inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse) #without the 'return', cachSolve(x) with a null cache won't return anything
}
