## This pair of functions will cache the inverse of an invertible matrix

## makeCacheMatrix will  cache the inverse
makeCacheMatrix <- function(x = matrix(nrow=n, ncol=n)) {
  im <- NULL
  set <- function(y = matrix(nrow=n, ncol=n)) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) im <<- solve(x)
  getsolve <- function() im
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve will compute the inverse of the invertible matrix 
## returned by makeCacheMatrix,unless already calculated which 
##it will then retrieve from the cache
cacheSolve <- function(x = matrix(nrow=n, ncol=n)) {
  im <- x$getsolve()
  if(!is.null(im)) {
    message("getting cached inverse matrix data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
}
