## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( mat = matrix() ) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    mat
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(mat) ) {
    message("getting cached data")
    return(mat)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  mat <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(mat)
  
  ## Return the matrix
  mat
}
