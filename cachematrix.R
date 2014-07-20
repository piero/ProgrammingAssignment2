# These functions define a mechanism to declare a special "matrix" that
# caches its own inverse.

makeCacheMatrix <- function( innerMatrix = matrix() ) {
  # Create a CacheMatrix from a "normal" matrix.
  #
  # A CacheMatrix can be accessed through these functions:
  #
  #   set(x) : Update the internal matrix and set the inverse to NULL.
  #   get()  : Return the internal matrix.
  #   setInverse(inv) : Cahcw the inverse.
  #   getInverse()    : Return the cached inverse (or NULL, if not cached).
  #
  # Args:
  #   x: A normal matrix. Default is an empty matrix.
  #
  # Returns:
  #   A new CacheMatrix, whose inverse is set to NULL until it's first computed.
  
  inverse <- NULL
  
  set <- function(m) {
    innerMatrix <<- m
    inverse <<- NULL
  }
  
  get <- function() innerMatrix
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'.
  #
  # Args:
  #   x: The CacheMatrix we want to invert.
  #
  # Returns:
  #   The matrix inverse of 'x'.
  inv <- x$getInverse()
  
  if ( ! is.null(inv) ) {
    return( inv )
  }
  
  # Compute and cache the inverse
  data <- x$get()
  inv <- solve( data, ... )
  x$setInverse( inv )
  inv
}
