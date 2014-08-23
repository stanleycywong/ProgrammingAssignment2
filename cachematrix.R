## There are two functions in this R script - makeCacheMatrix and cacheSolve
## These functions will create a matrix object and cache its inverse. If the matrix has not been updated, then the
## inverse will be cached.

## makeCacheMatrix function - to create a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	# set initial value of inverse matrix
	i <- NULL
	
	# reset the values in the matrix and its inverse
	setMatrix <- function(y) {
		x <<- y
		i <<- NULL
	}

	# show the matrix
	getMatrix <- function() m
	
	# store the inverse matrix
	setInverse <- function(inverse) i <<- inverse
	
	# show the inverse matrix
	getInverse <- function() i
	
	# create a list of functions
	list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function - to calculate the inverse of a square invertible matrix
cacheSolve <- function(x, ...) {

	# get the inverse matrix
	i <- x$getInverse()
	
	# get the cached inverse if the original matrix is not reset
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	
	# get the matrix
	data <- x$getMatrix()
	
	# compute the inverse matrix
	i <- solve(data, ...)
	
	# store the inverse matrix
	x$setInverse(i)
	
	# return the inverse matrix
	i
}
