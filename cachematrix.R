## Put comments here that give an overall description of what your
## functions do

## Function to make 'cache' matrix from a given matrix

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL                                      ## Initialize the inverse property
  
  set <- function( matrix ) {                     ## Method to set the matrix

    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    m         ## Return the matrix
  }
  
  setInverse <- function(inverse) {                  ## Method to set the inverse of the matrix
    i <<- inverse
  }
  getInverse <- function() {                      ## Method to get the inverse of the matrix

    i                                           ## Return the inverse property

  }
  
  list(set = set, get = get,                ## Return a list of the methods
       setInverse = setInverse,
       getInverse = getInverse)
}



## Function to calculate inverse of 'cache' matrix
##If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()                      ## Return a matrix that is the inverse of 'x'
  
  
  if( !is.null(m) ) {                    ## Just return the inverse if its already set
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()                      ## Get the matrix from our object
  
  m <- solve(data) %*% data           ## Calculate the inverse using matrix multiplication
  
  x$setInverse(m)                    ## Set the inverse to the object

  m                                 ## Return the matrix

}
