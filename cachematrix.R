# makeCacheMatrix is a function that creates a special "matrix" object that
# can cache its inverse, while the cacheSolve function computes the inverse
# of the special "matrix" returned by the makeCacheMatrix function.

# makeCacheMatrix is a function that calls on other embedded functions. 
# Its purpose is to store a matrix and a cached value of the inverse 
# of the matrix. It consist of the following functions:
# * setMatrix       sets the value of a matrix
# * getMatrix       gets the value of a matrix
# * cacheInverse    caches the value of the inverse of the matrix
# * getInverse      gets the cached value of the inverse of the matrix
#
# "x = matrix()" argument in the makeCacheMatrix function creates a matrix
# that is available in the environment of the function, but not reachable 
# from the global environment.

makeCacheMatrix <- function(x = matrix()) {
  
  # This holds the cached value or NULL if nothing is cached
  # It is set to NULL initially as nothing is cached
  cache <- NULL
  
  # Sets a matrix
  setMatrix <- function(newValue){
      x <<- newValue
      # Cache is flushed since the matrix is assigned a new value
      cache <- NULL
  }
  
  # Returns the stored matrix
  getMatrix <- function(){
      x
  }
  
  # The given argument is cached
  cacheInverse <- function(solve){
      cache <<- solve
  }
  
  # Gets the cached value
  getInverse <- function(){
      cache
  }
  
  # Returns a list with each named element being a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# This function calculates the inverse of a special "matrix" created using the
# previously defined makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  # Gets the cached value
  inverse <- x$getInverse()
  
  # Returns a cached value if it exist
  if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
  }
  
  # If it does not exist, get the matrix, calculate the inverse and store it
  # in the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  # Returns the inverse
  inverse
}
