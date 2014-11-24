## Functions to cache the inverse of a matrix


## Make a  matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse object
  inv <- NULL
  
  ## Set matrix method
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Get Input Matrix 
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Set Inverse Matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get Inverse Matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return get set methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Derive the inverse of the matrix returned from makeCacheMatrix
## If the inverse has already been calculated and there is no change, then 
## the below cacheSolve method will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  
  ## Return inverse of 'x' when x is a matrix
  ipmat <- x$getInverse()
  
  ## If already set, return the inverse
  if( !is.null(ipmat) ) {
    message("getting cached data")
    return(ipmat)
  }
  
  ## Retrieve the matrix data from the object
  data <- x$get()
  
  ## Calculating inverse using matrix multiplication
  ipmat <- solve(data) %*% data
  
  ## Set the inverse back to the object
  x$setInverse(ipmat)
  
  ## Return the matrix
  ipmat
}