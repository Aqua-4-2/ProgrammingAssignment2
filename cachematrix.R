#In this function we create a cache of a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) { #Creating a cache
	inv <- NULL 
	setMatrix <- function(y){ #Store matrix
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function(){ #Retrieve stored matrix
		x
	}
	setInverse <- function(inverse){ #Store inverse of matrix
	inv <- inverse
	}
	getInverse <- function(){ #Retrieve stored inverse of matrix 
		inv
	}
	cache <- list(setMatrix=setMatrix, getMatrix=getMatrix, 
			setInverse=setInverse, getInverse=getInverse)
}

#This function gives the inverse of the input matrix.
#It first checks if the input matrix is the same as the cache matrix or not.
#If they are the same, then the inverse will be same as well and so it returns
#the cached inverse of the matrix, alongwith a message "getting cached data".
#If they are different, it calculates the inverse of the input matrix,
#stores it in the cache and returns it.

cacheSolve <- function(x=matrix()) {
	if(nrow(x) != ncol(x)){ #Check if matrix is square
	message("Please enter a square matrix")
	}   
	makeCacheMatrix(x) #Call cached data
	if(identical(x,cache$getMatrix())){ #If the matrix is in cache
		message("Getting cached data")
		return (cache$getInverse()) #Return cached inverse
	} 
	else{ #For new matrix
		inv <- solve(x) #Calculate inverse
		cache$setMatrix(x) #Store matrix in cache
		cache$setInverse(inv) #Store inverse of matrix in cache
		return (inv) #Return calculates inverse
	}
}
