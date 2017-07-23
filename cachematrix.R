## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_Invr <- function(inv) inverse <<- inv
  get_Invr <- function() inverse
  list(set=set, get=get, set_Invr=set_Invr, get_Invr=get_Invr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$get_Invr()
  if(!is.null(inverse)) {
    message("Going to get cached data.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$set_Invr(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
