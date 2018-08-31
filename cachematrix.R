## Put comments here that give an overall description of what your
## functions do

## A function to create a special matrix which is invertable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  Set <- function(y){
    x <<- y
    inv <<- NULL
  }
  Get <- function() x
  SetInverse <- function(inverse) inv <<- inverse
  GetInverse <- function() inv
  list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## Computes the inverse of the output made by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$GetInverse()
  if(!is.null(inv)) {
    message("getting cache")
    return(inv)
  }
  data <- x$Get()
  inv <- solve(data, ...)
  x$SetInverse(inv)
  inv
}
##Check Program for Functional Capability
check <- matrix(rnorm(16), 4,4)
test1 <- makeCacheMatrix(check)
cacheSolve(test1)

