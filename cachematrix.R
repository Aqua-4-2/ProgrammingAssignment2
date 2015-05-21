#In this function we create a cache of a matrix and its inverse. 
#If a new matrix is input it calculates the inverse.
#If the previously used matrix is input it calls cached data.

makeCacheMatrix <- function(x = matrix()) { #Creating a cache
	inv <- NULL 
	setMatrix <- function(y){ #Store matrix
		x <- y
		inv <- NULL
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
