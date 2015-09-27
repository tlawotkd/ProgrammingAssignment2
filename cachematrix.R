## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This is basically the same thing as the first function in the
## example, except that it now creates a matrix instead of a vertor
## To use the casheSolve function, we first need to assign a matrix
## using the target matrix as input argument.

makeCacheMatrix <- function(x = matrix()) {
	result <- NULL 
		
      set <- function(y) {
		x <<- y
            result <<- NULL
		## <<- stands for "SUPERassignment", i.e. assign to global var.
	}
	get <- function() x

	## store the inversed matrix to the result var.
	setInv <- function(inverse) result <<- inverse
	## return result
	getInv <- function() result
	list(set = set, get = get,
		setInv = setInv,
		getInv = getInv)

}


## Now this function is the actual function calculating the inverse.
## But it depends on the above-written function to do solving/caching/loading
## as well as the vanialla R solve() function to calculate the matrix.

## FOR REF
## solve(x) returns the inverse of x
## for matrix multiplaction (and not the element-wise mult.) use %*%
## END REF

cacheSolve <- function(x, ...) {
	
	## First try to call the above fuction's list to call its 'chached matrix' ,
	## if it is same as x, just return the cashed one
	m <- x$getInv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	## if not cached, solve x (i.e. calculate the inverse)
	message("Could not get cached data, calculating inverse now")
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m
}


## BELOW IS THE TEST CODE

##source("cachematrix.R")
##x<-makeCacheMatrix(matrix(1:4, 2, 2))
##x$get() ## this will just print the stored matrix(1:4,2,2)
##x$getInv() ## NULL, because we didn't solve it yet.
##cacheSolve(x) ## now, the solution will be calculated AS WELL AS STORED
##x$getInv() ## calling the stored inverse matrix, because we didn't solve it yet.
##cacheSolve(x) ## solution will be "called" from the stored fuction








