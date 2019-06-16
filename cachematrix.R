# Coursera R programming course, assigment 2
# Patric Gustafsson

# Function that returns a vector of functions for caching the matrix inverse:
# setInverse: Sets the inverse to some value
# getInverse: Gets the inverse, if there is one calculated
# set: Stores the matrix for later calculations
# get: Gets the stored matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setInverse <- function(inv) { inverse <<- inv }
  getInverse <- function() { inverse }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Calulates the inverse of a matrix, using the cached value if needed
# Returns: the inverse of the matrix x
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}