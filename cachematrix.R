## The fucntions below are for the coursera.org week 3 porgramming 
## assignment 2. They work together to create the inverse of a matrix 
## subsequently solve the inverse of said matrix.


## This function "makeCacheMatrix" creates a matrix object that caches 
## the inverse of the matrix object created.


makeCacheMatrix <- function(x = matrix()) 
		{
		inv <- NULL
		set <- function(y) 
        		
			{
			x <<- y
			inv <<- NULL
			}
        		
		get <- function() x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = set, get = get, setInverse = setInverse, 		getInverse = getInverse)
		}


## This function "cacheSolve" attempts to compute the inversed special 
## matric object created by the "makeCacheMatrix" funtion. 


cacheSolve <- function(x, ...) 
		{
		inv <- x$getInverse()
		if (!is.null(inv)) 
		
			{
			message("retieving and calculating")
			return(inv)
			}
		
		matrixI <- x$get()
		inv <- solve(matrixI, ...)
		x$setInverse(inv)
		inv
		}
