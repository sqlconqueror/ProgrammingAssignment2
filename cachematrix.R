## These fuctions calculate the inverse of a matrix
## functions:
##    1. makeCacheMatrix: Creates the special "matrix"
##    2. cacheSolve:      Uses makeCacheMatrix to calculate the inverse or return the cached version

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #create empty matrix
  m <- NULL
  #set function for matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get function for matrix
  get <- function() x
  #set function for inverse matrix
  setInverse <- function(solve) m <<- solve
  #get function for inverse matrix
  getInverse <- function() m
  #return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" or 
## the cached value of an already calculated inverse
cacheSolve <- function(x, ...) {
  #get value of inverse matrix
  m <- x$getInverse()
  #check if value exists and return value if True
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #for new matrix evaluate and set inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  #return value
  m
}