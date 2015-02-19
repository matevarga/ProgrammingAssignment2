# The vector example shown in the assignment
# description shows a very bad practice - 
# namely coupling data that depend on each other
# AND breaking encapsulation by exposing
# functions to access them individually.
# It is easy to just set the mean to something
# that does not equal to the mean of the vectol.
#
# Therefore I will try to properly encapsulate
# the inverse calculation inside the
# cached matrix 'object' by NOT exposing
# a setter for the inverse, but delegating
# inverse calculation to the object itself.



#
# This function creates a tuple (list) of
# - a matrix
# - functions to get/set the value of the matrix
# - function to calculate the inverse of the matrix and store results
# The cached inverse is invalidated when the matrix is re-set.
# 
makeCacheMatrix <- function(x = matrix()) {
  
  # Utility func to decide whether two matrices are equal
  matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  }
  
  # cache for the inverse
  inverse <- NULL
  
  # sets the matrix if it is different than the
  # previous value and clears the cache
  set <- function(y)
  {
    # We only set x and reset _inverse if the new matrix is different
    if (!matequal(x,y))
    {
      x <<- y
      inverse <<- NULL
    }
  }

  # returns the value of the matrix
  get <- function() x

  # calculates the inverse if it has not been
  # calculated yet, then returns it (or the cached val)  
  calculateAndCacheInverse <- function()
  {
    if (is.null(inverse))
    {  
      message("Inverse not cached, calculating it")
      inverse <<- solve(x)
    }
    inverse
  }
  list (
    set = set,
    get = get,
    calculateAndCacheInverse = calculateAndCacheInverse)
}

#
# Returns the inverse of the matrix by delegating
# the call to the matrix itself 
#
cacheSolve <- function(x, ...) {
  x$calculateAndCacheInverse()
}
