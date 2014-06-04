## In this file, two functions are defined:
##  - makeCacheMatrix
##  - cacheSolve
## Matrix inversion is usually a costly computation. Therefore, 
## caching the inverse of matrix may be useful, and the functions do it.


# The following function do:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix.
# 4. get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv_mat) inv <<- inv_mat
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv, 
		getinv = getinv)
}


# cacheSolve return the inverse of matrix. 
# But if already computed the inverse, skip the calculation and return 
# the cached inverse. Otherwise, compute the inverse and set the inverse 
# into the Cachematrix, and then return the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
