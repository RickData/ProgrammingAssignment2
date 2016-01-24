## This R file has the solutions to Assignment 2 of
## the R Programming Course

## This function creates a matrix and holds an internal
## variable. It is filled when an inverse matrix is computed

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



## This function computes the inverse matrix when it is not filled
## otherwise ir gets a previously computed value, It advices 
## when the cached value is used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
