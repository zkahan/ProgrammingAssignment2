## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. 

## We are using following two functions to caching the inverse of a matrix:


## 1.  makeCacheMatrix: This function creates a special "matrix" object
##     that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

		# Initialize the stored inverse value to NULL
	    inverse <- NULL

	    # Set the matrix data
	    set <- function(y) {
	        x <<- y
	        inverse <<- NULL
	    }

	    # Get the matrix data
	    get <- function() x

	    # Set the inverse
	    setinverse <- function(inv) inverse <<- inv

	    # Get the inverse
	    getinverse <- function() inverse

	    # Return a list of above functions
	    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

}


## 2.  cacheSolve: This function computes the inverse of the special
##     "matrix" returned by "makeCacheMatrix" above. If the inverse has
##     already been calculated (and the matrix has not changed), then
##     "cacheSolve" should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the "solve"
## function in R.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## First check if the inverse is already cached
        ## If cached, we return the cached data 
	    inv <- x$getinverse()
	    if(!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
	    }

	    ## If not cached, we compute the inverse here.

	    ## Get the matrix into data
	    data <- x$get()

	    ## Compute the inverse with "Solve" function
	    inv <- solve(data, ...)

	    ## Cache the inverse data
	    x$setinverse(inv)

	    ## Return the inverse data
	    inv

}
