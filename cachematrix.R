## These functions create a special vector for caching matrix inverse and using this vector to calculate the inverse.

#' Creates a vector containing a cacheable matrix inverse
#' @param x: a square invertible matrix
#' @return: a list containing functions to
#'  1. set the matrix \code{x}
#'  2. get the matrix \code{x}
#'  3. set the inverse
#'  4. get the inverse
#'  this list is used as the input to cacheSolve()
#' @example 
#'  makeCacheMatrix(matrix(c(1,2,1,3), nrow = 2, ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Calculates matrix's inverse or gets its cached value
#' @param x: output of makeCacheMatrix()
#' @return: inverse of the original matrix input to makeCacheMatrix()
#' @example 
#'  cacheSolve(makeCacheMatrix(matrix(c(1,2,1,3),nrow = 2, ncol = 2)))
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  # calculate matrix inverse
  i <- solve(data, ...)
  x$setinverse(i)
  i
}