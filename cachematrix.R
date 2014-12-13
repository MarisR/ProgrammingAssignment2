# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.

# function makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  # initialize the inverse matrix value
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  
  # get the value of the inverse
  get_inverse <- function() inv
  
  # return a list of all the above functions
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## function cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. 
## It first checks if the inverse has already been calculated. 
## If yes, gets the inverse from the cache
## If no, it calculates the inverse of the matrix 
## and sets the value in the cache via the setinv function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

  # check if the inverse is already cached,
  # if yes, get the inverse from the cache
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not in cache, get the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # cache the inverse of the matrix
  x$set_inverse(inv)
  
  # and return the result
  inv
}
