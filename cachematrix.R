# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves matrix to variable x and the inverse to variable m

makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get, setsolve=setsolve, 		
             getsolve=getsolve)
}

# cacheSolve
# Function to get the inverse matrix from a matrix created by makeCacheMatrix.
# It takes the matrix as an argument 'x' and checks if the inverse is cached:
# If it is returns the cached value; 
# If not, calculates the inverse for the matrix saved in the 'x', 
# saves it into 'x' cache using method 'setSolve', and returns the result.

cacheSolve <- function (x=matrix(), ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
                