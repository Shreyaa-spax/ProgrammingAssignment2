# makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when matrix changes
  }
  
  # Get matrix value
  get <- function() x
  
  # Set inverse value
  setinverse <- function(inverse) inv <<- inverse
  
  # Get inverse value
  getinverse <- function() inv
  
  # Return list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve function
cacheSolve <- function(x, ...) {
  # Retrieve cached inverse
  inv <- x$getinverse()
  
  # If inverse is already computed, return it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse of the matrix if not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
