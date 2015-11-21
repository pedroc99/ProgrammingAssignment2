## Put comments here that give an overall description of what your

## functions do

## Write a short comment describing this function

## the first function makeCacheMatrix creates a list containing 4 functions.
## these are:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverted matrix
##	4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	## this initializes a NA matrix with the same number of columns and rows as the input matrix
	invX <- matrix(NA, ncol = ncol(x), nrow = nrow(x))
	set <- function(y) {
		x <<- y
		invX <<- matrix(NA, ncol = ncol(x), nrow = nrow(x))
	}
	get <- function() x
	setinv <- function(inverted) invX <<- inverted
	getinv <- function() invX
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## This calculate the inverse of a matrix using solve()
## if it has been calculated already then it returns the inverse matrix in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getinv()
        if(!all(is.na(invX))) {
            message("getting the cached inverse matrix")
            return(invX)
        }
        dataM <- x$get()
        invX <- solve(dataM,...)
        x$setinv(invX)
        invX
}
