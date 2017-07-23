## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
#  set the value of the matrix
#  get the value of the matrix
#  set the value of inverse of the matrix
#  get the value of inverse of the matrix
#  Purpose of the function is to create variables , functions and cached variables,so we cant save or fetch the data


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
#It returns inverse of matrix.If inverse is already calculated ,
#it just returns the cached result
#if not already computed then it compute the inverse first then cache's it

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inverse <- x$get_Invr()
  if(!is.null(inverse)) {
    message("inverse to get cached data.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$set_Invr(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
