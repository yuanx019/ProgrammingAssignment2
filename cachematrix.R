## The first function, makeCasheMatrix creates a special "matrix", 
## which is actually a list, to cashe its inverse matrix.

## "set" function sets the value of the matrix.
## "get" function gets the value of the matrix.
## "setinv" function sets the value of the inverse matrix.
## "getinv" function gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setinv <- function(solve)inv <<- solve
	getinv <- function()inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The second function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the setinv function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!if.null(inv)){
		message("getting cashed data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
