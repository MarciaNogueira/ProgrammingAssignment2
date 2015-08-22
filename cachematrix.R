## This file contains two functions which calculate and cache the inverse
## of a given matrix

## The function makeCacheMatrix creates a special "matrix" object that can
## cache the inverse of the given matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function () x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The function cacheSolve calculates the inverse of the special "matrix" object 
## created with the function makeCacheMatrix, unless it has already been calculated,
## in which case it skips the computation and returns the value already stored 
## in the cache. 
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setsolve(inv)
  inv
}

