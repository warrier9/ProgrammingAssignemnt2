## Two main functions makeCacheMatrix() and cacheSolve() are built. 
## makeCacheMatrix() creates a matrix object that can cache its inverse
##cacheSolve() calculates the inverse of the matrix returned by the makeCacheMatrix()
## function.

## x is the square inverse matrix
## create a list which is used as the input for the cacheSolve function
## the list contains functions to set and get the matrix followed by setting
## and returning the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) mat <<-solve
  getMatrix <- function() m
  list(set=set, get=get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
}

## cacheSolve() function returns the inverse of the matrix 
## inputted to the makeCacheMatrix() function. If the inverse is already 
## calculated it extracts the matrix from the cache to skip computing again.

cacheSolve <- function(x = matrix(), ...) {
  mat <- x$getMatrix()
  if(!is.NULL(mat)) {
    message("getting cached data")
    return(mat)
  }
  matrixData <- x$get()
  mat <- solve(matrixData, ...)
  x$setMatrix(mat)
  mat
}
