## In conjunction these functions create, cache and return the inverse of
## a user inputted matrix

## Creates a special matrix with the ability to:
##	Set the values of the matrix
##	Get the values of the matrix
##	Set the Inverse of the matrix
##	Get the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)

}


## Calculates and returns the inverse of a matrix
## If the inverse has already been cached it retrieves and returns cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i) ) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
}
