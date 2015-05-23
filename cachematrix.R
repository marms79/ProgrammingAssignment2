## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that will create a special matrix object
## later on, we'll use cacheSolve to compute the inverse of that matrix.
## If the cache exists for that matrix inverse having already been created, 
## cacheSolve will find it in the cache and return it, instead of re-calculating it.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inverse)      inv_x <<-inverse
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)       
}


## Write a short comment describing this function'
## cacheSolve is a function that returns the inverse the inverse of
## a matrix A created by the function makeCacheMatrix.
## It looks to see if a cached inverse is available, then cacheSolve retrieves it
## but if it's not available, it will calculate, cache and return the inverse.

cacheSolve <- function(x, ...) {

        inv_x <- x$getinv()
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinv(inv_x)
                
                return(inv_x)
                
## Return a matrix that is the inverse of 'x'
        }
}