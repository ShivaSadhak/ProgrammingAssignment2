## Below are two functions which Compute the inverse of a matrix and Cache it.

## First function creates a special matrix object, which is really a list containing a function to 
## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. Set the value of inverse
## 4. Get the value of inverse
makeCacheMatrix <- function(mtrx = matrix()) {
    inverse <- NULL
    set <- function(x) {
    mtrx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(mtrx, ...) {
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  data <- mtrx$get()
  invserse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
}
