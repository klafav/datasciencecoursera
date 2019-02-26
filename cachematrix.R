## The makeCacheMatrix variable creates a special matrix
## This function will cache the inverse of the object
makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(z) {
    x <<- z
    inverse <<- NULL
  }
get <- function() x
setInverse <- function() inverse <<- solve(x)
getInverse <- function() inverse
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}
## This function computes the inverse of the above special matrix
## And Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
              return(inverse)
  }
data <- x$get()
inverse <- solve(data, ...)
x$setInverse()
inverse
}
