## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## It does the following
## 1. gives a function to set the value of matrix
## 2. gives a function to get the value of matrix
## 3. gives a function to set the value of matrix inverse
## 4. gives a function to get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initalize the matrix
  matrixInverse <- NULL

  ## function to set the matrix
  set <- function(y){
    x <<- y
    matrixInverse <<- NULL
  }

  ## function to get the matrix
  get <- function() x

  ## function to set the inverse
  setinverse <- function(inverse) matrixInverse <<- inverse

  ## function to get the inverse
  getinverse <- function() matrixInverse

  ## list of functions created above
  list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes matrix inverse of the special matrix 
## returned by makeCacheMatrix function above. It returns cached
## results if they are available or computing the inverse using 
## solve function and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Try and retrieve the inverse
  matrixInverse <- x$getinverse()

  ## If cached inverse found then return it
  if (!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  ## cached inverse not found, then compute and cache it.
  mdata <- x$get()
  matrixInverse <- solve(mdata)
  x$setinverse(matrixInverse)

  matrixInverse
}
