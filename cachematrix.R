## This script has two main functions. The first one is makeCacheMatrix which 
## takes a parameter as a matrix for which the inverse is required and creates 
## the matrix which can be read by the CacheSolve function. 

## The makeCacheMatrix will create the matrix. It takes an input as the matrix 
## so that cacheSolve can read it from cache if it exists

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function first checks if there is a cached version of the inverse
## If there is a cached version, it will send a message that the cached
## version is being obtained. It uses solve to obtain the inverse of the matrix 
## and it sends the value of the inverse into m through the setinv function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
