## these two functions compute and cache the inverse of a matrix. 

## This function builds functions to store the matrix and it's inverse and functions to retrieve the two, wraps those
## functions in a list called 'lista' in the global environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  lista<<-list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## when the list 'lista' that was built in the preceding function is passed to this function, it checks whether the 
## inverse is available in the cache and if so, returns the value of the inverse but if the inverse is not available, it 
## calculates the inverse and returns the same

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  library(MASS)
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
