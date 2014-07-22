###CACHING THE INVERSE OF A MATRIX###

## 1. Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## Initialise the inverse property
	inv <- NULL

	## Set the matrix
	set <- function(matrix) {
		x <<- matrix
		inv <<- NULL
	}

	## Get the matrix
	get <- function() x

	## Set the inverse of the matrix
	setInv <- function(inverse) {
		inv <<- inverse
	}

	## Get the inverse of the matrix
	getInv <- function() inv

	## Return a list
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## 2. Computes the inverse of the special matrix returned by "makeCacheMatrix". If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
 
	## Return x's inverse matrix
	x <- x$getInverse()

	## Eeturn the inverse
	if( !is.null(x) ) {
		return(x)
	}

	## Get the matrix
	data <- x$get()

	## Calculate the inverse
	x<-solve(data, ...)
	
	
	## Set the inverse
	x$setInverse(x)

	## Return the matrix
	x
}
