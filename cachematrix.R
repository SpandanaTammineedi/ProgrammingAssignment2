## Caching the inverse of a matrix using functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix is a function that creates a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinv <- function(SMatrix) inverse <<- SMatrix
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve is a function that uses the matrix returned by makeCasheMatrix and calculates its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inv <- SMatrix(data)
  x$setinv(inverse)
  inverse
}
