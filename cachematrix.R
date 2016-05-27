##Computing the inverse of a matrix is often a costly computation. The functions below cache the inverse of a matrix, in order to avoid to compute it repeatedly.


##This function creates a special object, which is a list containing a function to set the matrix, get the matrix, set the inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(matr) {
		m <<- matr
		minv <<- NULL
		}
		get <- function() m
		setinv <- function(inv) minv <<- inv
		getinv <- function() minv
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function computes the inverse of the special "matrix" created with the function above. It first check to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse and sets its value in the cache via the setinv function.  

cacheSolve <- function(x, ...) {
	minv <- m$getinv()
	if(!is.null(minv)) {
		message("getting cached data")
		return(minv)
	}
	matrice <- m$get()
	minv <- solve(matrice)
	m$setinv(minv)
	minv

}
