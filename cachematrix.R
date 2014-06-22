

## Create a matrix for which to solve the inverse (must be square)
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
         x <<- y
    	 m <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(solve) m <<- solve
  	getinverse <- function() m
  	list(set = set, get = get,
       	     setinverse = setinverse,
             getinverse = getinverse)
}



## Write a function to either solve the inverse of 'x' or pull
##  the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
  	if(!is.null(m)) {
    	  message("getting cached data")
    	  return(m)
  	}
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	m
}

