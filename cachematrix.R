## Week 3 assignment to cache and compute the inverse of a matrix.
## Console message will indicate whether the inverse is retrieved from the cache.
## The logic for checking whether a cached inverse exists has been inverted compared to the given cache mean example to make code more concise in my opinion, especially when no console output is needed.
## To use the functions, call makeCacheMatrix with an invertible matrix, and call cacheSolve to cache and compute the inverse of the matrix. If cacheSolve is called again, the the inverse will be retrieved from the cache.

## This function takes in a matrix object and returns a list of functions. It can cache the inverse of its input

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the output of makeCacheMatrix. 
## If the inverse has been computed already, then the computed value is retured as output directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(is.null(inverse)){
        	message("Matrix inverse is computed on the fly.")
        	matrix <- x$get()
        	inverse <- solve(matrix, ...)
        	x$setInverse(inverse)
        }
        else {
        	message("Matrix inverse is retrieved from cache.")
        }
        inverse
}
