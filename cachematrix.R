makeCacheMatrix <- function(x = matrix())
{
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    #cache is flushed
    cache <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() 
    x
  # cache the given argument 
  setInverse <- function(solve)
    cache <<- solve
 # get the cached value
  getInverse <- function() 
    cache
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}  

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

 cacheSolve <- function(x, ...) {
  # get the cached value
  cache <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(cache)) {
    message("getting inverse of matrix from cache")
    return(cache)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$getMatrix()
  cache <- solve(data)
  x$setInverse(cache)
  
  # return the inverse
  cache
}