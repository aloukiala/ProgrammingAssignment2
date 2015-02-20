## Put comments here that give an overall description of what your
## functions do
## "For this assignment, assume that the matrix supplied is always invertible."
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) im <<- solve
  getSolve <- function() im
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getSolve()
  if(!is.null(im)) {
    message("getting cached matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setSolve(im)
  im
}
