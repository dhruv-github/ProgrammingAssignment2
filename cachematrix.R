## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## returns a list with functions to set value of matrix, get value of matrix
## and set value of the inverse of the matrix and get that value  (6/17/15)  

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function
## Function which decides whether to get the inverse of the matrix from cache or compute the 
## inverse of the matrix   (6/17/15)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
        	message("getting cached data")
        	return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}


### test ### 
a=matrix(rnorm(64),nrow=8,ncol=8)
mcm=makeCacheMatrix(a)
ainv=cacheSolve(mcm)
a%*%ainv ### verify
ainv = cacheSolve(mcm)  ### now you get mesg 'getting cached data' 



