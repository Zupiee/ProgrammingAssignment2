## The following pair of functions cache the inverse of a matrix.

## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL  				# mi matrix to store the inverse matrix
	set <- function(y){			# assigns value to the matrix in parent environment
		x <<- y
		mi <<- NULL
	}
	get <- function() x			# returns the matrix
	setinverse <- function(i) mi<<-i	# assigns value to the inverse matrix in parent environment
	getinverse <- function() mi 	    	# retruns the inverse matrix 
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}


## Calculates the inverse of the matrix and if inverse is already calculated then retrieve from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){   			
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
