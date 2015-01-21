## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix whose inverse is to be calculated
## set -> sets the matrix
## get -> gets the matrix
## setsolve -> sets the value of the inverse of the matrix
## getsolve -> gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
		
	}
	
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <-x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m

}
