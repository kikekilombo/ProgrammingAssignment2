## Put comments here that give an overall description of what your
## functions do

# Funtion: makeCacheMatrix
#   Creates a structure that stores a matrix and its inverse. This 
#   function does not calculates the inverse of the metrix, just 
#   stores it for cached access
#
# Args:
#   m: Nonsingular square matrix
#
# Returns:
#   A cached matrix that is a list of functions to get and set the matrix and its inverve
#       Functions of a cached matrix:
#           get -> Returns the underlying matrix (m)
#           set -> Sets the underlying matrix and set the inverse to NULL
#           getinverse -> Returns the inverse of the matrix if exist or NULL
#                         otherwise
#           setinverse -> Set the inverse matrix (i)

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(matrix) {
        # Sets the matrix
        m <<- matrix
        # Reset the inverse to NULL
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # Returns the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Funtion: cacheSolve
#   Calcuates the inverse of a cached matrix using the stored matrix if
#       available or calculating and stroing the inverse matrix otherwise. 
#       This function uses the function solve to do the inverse matrix calculation
#
# Args:
#   x: Nonsingular square cached matrix (Created using the function makeCacheMatrix)
#   The rest of the parameters in the call are being passed to the solve function
#
# Returns:
#   The inverse of matrix

cacheSolve <- function(x, ...) {
    # Get the cached inverse  
    i <- x$getinverse()
    if(!is.null(i)) {
        # if the cached inverse exist, displays a message and returns the cached inverse matrix
        message("getting cached data")
        return(i)
    }
    # if the cached inverse does not exist, calculates the inverse matrix
    data <- x$get()
    i <- solve(data, ...)
    # Stores the calculated inverse matrix
    x$setinverse(i)
    
    # Returns the inverse matrix
    i
}
