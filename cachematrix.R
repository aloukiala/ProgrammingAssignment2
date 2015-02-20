## Functions to create and handle inversible matrix.
## makeCacheMatrix creates matrix that can store its inverse matrix
## cacheSolve is function that handels makeCacheMatrix created matrix
## caches inverse matrix when calculated.
##
## "For this assignment, assume that the matrix supplied is always invertible."


## This function creates a matrix with setter and getter methods. 
## The function stores the solve functions output, the inverse matrix in 
## cahce and it can be returned with getSolve if it has been calculated.
## Input -> matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix (im)
  im <- NULL
  # Set the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  # Get stored matrix
  get <- function() x
  # Set solve result  
  setSolve <- function(solve) im <<- solve
  # Get solve output
  getSolve <- function() im
  
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## This function solves the inverse matrix of given input.
## The input is assumed to be inversible.
## Function reads the solve result from cache if it has been
## already calculated for this function
## Input-> cache matrix
cacheSolve <- function(x, ...) {
  # Check if the solve has been cached
  im <- x$getSolve()
  if(!is.null(im)) {
    return(im)
  }
  # We need to solve the matrix...
  data <- x$get()
  # and store the result for future use
  im <- solve(data, ...)
  x$setSolve(im)
  # return the inverse matrix 
  return(im)
}
