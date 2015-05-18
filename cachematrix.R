## Put comments here that give an overall description of what your
## functions do

## This function get a matrix x and store it as cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_m <<- inverse
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function gets a matrix that was stored as cacheMatrix and calculate its inverse. 
##If the inverse was calculated before, it uses the cached value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinv(inv_m)
  inv_m
}
