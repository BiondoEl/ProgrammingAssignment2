#function to define matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #cached inverse
  inv <- NULL
  
  #matrix get / set functions
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #inverse of matrix get / set functions
  getinv <- function() inv
  setinv <- function(solve) inv <<- solve
  
  #return list of all functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


#function to retrieve inverse of matrix (cached if present)

retrievematrixinv <- function(x, ...) {
  inv <- x$getinv()
  
  #return cached inverse if present
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  # define inverse of matrix
  matrixdata <- x$getinv()
  inv <- solve(matrixdata, ...)
  
  #cache inverse of matrix
  x$setinv(inv)
  
  #return inverse of cached matrix
  return(inv)
}

