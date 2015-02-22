## The pair of functions will cache the inverse of a matrix.
## This script assumes that the matrix supplied is always 
## invertible

## The following function creates a special "matrix" object that 
## can cache its inverse. It does a list of things:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the 
## solve function in R. For example, if X is a square invertible 
## matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Sample runs:
## > mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
## > mat
##        [,1] [,2]
##  [1,]    4    7
##  [2,]    2    6
## > mat2 <- makeCacheMatrix(mat)
## > cacheSolve(mat2)
##       [,1] [,2]
##  [1,]  0.6 -0.7
##  [2,] -0.2  0.4
## > cacheSolve(mat2)
## getting cached data
##       [,1] [,2]
##  [1,]  0.6 -0.7
##  [2,] -0.2  0.4
## >