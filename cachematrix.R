## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix takes a matrix and creates a function list
## that can cache the inverse matrix.
## It also checks if the given matrix is identical to that in memory, and
## retains matrices in memory if identical or reset memory if not.

makeCacheMatrix <- function(x = matrix()) {
	set <- function(M) {
		A <<- M
		AI <<- NULL
	}
	if(exists("A")) {
		if(all(dim(x) == dim(A)) && all(apply(x == A, c(1,2), all))) {
			message("INFO: Matrix is in cache, nothing changed")
		} else {
			message("INFO: New matrix given, reseting cache")
			set(x)
		}
	} else {
		set(x)
	}	
	get <- function() A
	setinv <- function(MI) AI <<- MI
	getinv <- function() AI
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes an object returned by makeCacheMatrix and
## either retrieves the inverse from cache, or computes the inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of that set in 'x'
	MI <- x$getinv()
	if(!is.null(MI)) {
		message("INFO: Inversed matrix retrieved from cache")
		return(MI)
	}
	M <- x$get()
	MI <- solve(M, ...)
	x$setinv(MI)
	MI
}

## Example run and output:
## > source('cachematrix.R')
## > x <- matrix(c(1,1,1, 3,4,3, 3,3,4), nrow=3, ncol=3)
## > a <- makeCacheMatrix(x)
## > cacheSolve(a)
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## > a <- makeCacheMatrix(x)
## INFO: Matrix is identical to that in cache
## > y <- matrix(c(4,3, 3,2), nrow=2, ncol=2)
## > a <- makeCacheMatrix(y)
## INFO: New matrix given, reseting cache
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(a)
## INFO: Inversed matrix retrieved from cache
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
