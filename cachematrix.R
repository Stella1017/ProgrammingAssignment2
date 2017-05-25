## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix will build several sub-functions, and will return
## a vector, which contains four functions: set(), get(), setInverse(), and getInverse()
## It also includes two data objects, x is the arguement of makeCacheMatrix, 
## and iv records the inverse matrix of 'x'
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL 
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) iv <<- solve
  getInverse <- function() iv
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## In the function cacheSolve, the arguement of this function is a vector.
## The function will first test whether a inverse of 'x' is already calculated
## and cached. For a new matrix, the x$getInverse would be NULL, then it gets
## the vector from the input, calculates the inverse of 'x', and finally returns
## the inverse of 'x' to the parent environment by printing the invert matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getInverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setInverse(iv)
  iv
}
