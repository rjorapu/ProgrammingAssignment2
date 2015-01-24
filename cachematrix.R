## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix x whose inverse is to be calculated
## set -> sets the matrix x
## get -> gets the matrix x
## s is used to store the solve results and to check if the matrix has changed
## it is the solve function.
## setsolve -> sets the value of the inverse of the matrix x
## getsolve -> gets the value of the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
		
	}
	
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix x created from makeCacheMatrix
## it first checks if the matrix was earlier saved (m is not null). If so, 
## it returns the value of the inverse from cache.
## Otherwise, it executes the solve function and returns the result

cacheSolve <- function(x, ...) {
	
     ## Return a matrix that is the inverse of 'x'
        
     s <- x$getsolve()
     
	    if(!is.null(s)){
		 message("getting cached data")
	     return(s)
	    }
	    
	data <-x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s

}
