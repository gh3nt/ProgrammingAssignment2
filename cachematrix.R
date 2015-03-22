## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function creates a list of 4 functions having common parent environment. 
# The matrix given as a function argument is stored in the common environment as a "x" variable. 
# The functions allow to get or set the "x" matrix, and to get or set the "inverted.matrix" value, 
# which is also stored in the common parent environment   

makeCacheMatrix <- function(x = matrix()) {
      
      #create empty variable for "inverted matrix"
      inverted.matrix <- NULL
      
      set <- function(y) {
            
            # assign matrix "y" to the "x" variable in the parent environment 
            x <<- y
            
            # when new matrix is assigned to "x", reset the "inverted.matrix" value in the parent environment
            inverted.matrix <<- NULL     
      }
      
      
      # return the matrix stored in the "x" variable 
      get <- function() x
      
      
      # assign "inverted.m" matrix to the "inverted.matrix" variable of the parent environment
      setinverted <- function(inverted.m) inverted.matrix <<- inverted.m
      
      # return the "inverted.matrix" variable
      getinverted <- function() inverted.matrix
      
      # return the list containing all 4 functions       
      list(set= set, get = get, setinverted = setinverted, getinverted = getinverted)
}


## Write a short comment describing this function

# the function returns a matrix that is the inverse of the matrix given as a parameter 
# (within the parent environment of the functions list created by the "makeCacheMatrix" function). 
# If the functions environment already contains the inverted matrix, 
# 'cacheSolve' function returns it. If not (inverted matrix is null), it calculates the reversed matrix, 
# stores it in the parent environment of the functions given as a "x" argument (through the "setinverted" function), and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      # check if "x" already contains inverted matrix, if yes - return it and finish function
      inverted <- x$getinverted()
      if(!is.null(inverted)){
            message("getting cached data")
            return(inverted)
      }
      
      # if "x" doesn't contain iverted matrix, get the basic matrix,  
      matrix <- x$get()
      # calculate the inverted matrix 
      inverted <- solve(matrix, ...)
      # store the inverted matrix in "x" 
      x$setinverted(inverted)
      # return inverted matrix
      inverted
}
