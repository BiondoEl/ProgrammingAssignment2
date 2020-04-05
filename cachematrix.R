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
  m <- x$get()
  inv <- solve(m, ...)
  
  #cache inverse of matrix
  x$setinv(inv)
  
  #return inverse of cached matrix
  return(inv)
}


#TEST

#matrixdata <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE) #defines test matrix
#matrixdata2 <- makeCacheMatrix(matrixdata) #adds test matrix as arg to function, creates cache

#matrixdata2$get() 
#> matrixdata2$get()
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4

#retrievematrixinv(matrixdata2) #retrieves inverse
# > retrievematrixinv(matrixdata2) --- no cache
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> retrievematrixinv(matrixdata2) --- cached data, gets message
#getting cached matrix
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
