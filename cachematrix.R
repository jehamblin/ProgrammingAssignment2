## This program creates functions that cache
## the inverse of a matrix; when the inverse
## of a matrix is request, a search is done
## to see if the inverse has already been
## computed.

## If not, the inverse is computed and the 
## value cached.
## If so, the cached value is returned.

## Create a "cache matrix", which is really
## just a list of functions to access the
## cached information.

makeCacheMatrix <- function(x = matrix()) {
  
  ## x is the original matrix
  ## m is its inverse
  
  m <- NULL
  
  ## set initializes the cache matrix
  ## (no inverse has been computed yet)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get accesses the original matrix
  get <- function() x
  
  ## setinv sets the value of the inverse
  setinv <- function(inv) m <<- inv
  
  ## getinv accessed the (cached) value of the inverse
  getinv <- function() m
  
  ## return the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve extends solve using the cacheMatrix functionality
## cacheSolve assumes its input is a cacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check to see if the inverse has been computed already
  m <- x$getinv()
  
  ## if so, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if not, get the original matrix and compute its inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## save this computed inverse and return it
  x$setinv(m)
  m
  
}
