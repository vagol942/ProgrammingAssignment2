## This functions describe an example of a "special" matrix that can cache its
## inverse and a function that can calculate the inverse taking into account
## if the inverse has already been calculated

## This functions takes a matrix (assumed to be invertible), and returns a list
## of the matrix itself plus its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	  
	  set <- function(y){
		x <<- y
		i <<- null
	  }
	  get <- function(){
		x
	  }
	  
	  setInverse <- function(inverse)
	  {
		i <<- inverse
	  }
	  
	  getInverse <- function() i
	  
	  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function calculates the inverse of a matrix, it takes a makeCacheMatrix has
## argument and calculates the inverse if it hasn't been done already.

cacheSolve <- function(x, ...) {
	  i <- x$getInverse()
	  if (!is.null(i))
	  {
		message("getting cached data")
		return(i)
	  }
	  data <- x$get()
	  i <- solve(data, ...)
	  x$setInverse(i)
	  i
}
