## The below two functions will save processing time by caching the inverse
## of a matrix the first time it is calculated, allowing you to recall it
## repeatedly without the cost of re-calculating the inverse.

## Essentially creates an object with two variables, 'x' and 'inv' and two
## methods, 'setinv' and 'getinv'

## Usage: create an instance of makeCacheMatrix, for example.
## mymat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
## mymat$get() should now return the 2x2 matrix with the values of 1, 2, 3 and 4.

## Next use the cacheSolve function which will call makeCacheMatrix to calculate
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Will call makeCacheMatrix to get the value of 'inv'
## If 'inv' is NULL, will call x$get() to get the stored matrix then use 
## solve() to caclulate the inverse and finally store the inverse by calling
## x$setinv(inv).

## Usage: call cacheSolve and pass it an instance of makeCacheMatrix.
##  mymat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
##  cacheSolve(mymat)
## The first call to cacheSolve(mymat) will return the inverse of mymat.
## The second call to cacheSolve(mymat) should also return the inverse of
## mymat, but will indiate that it has done so by retrieving cached data.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinv(inverse)
      x$getinv()
}
