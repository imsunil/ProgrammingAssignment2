## Along the lines of the make vector assignment makeCacheMatric function returns a list of 4 functions.
## And cachesolve function returns the inverse of the matrix from cache if its already comuted or calculates it if the matrix changes)

## MakeCacheMatrix function take a matrix as an argument and generates 4 functions that get and set the data
## and get, set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x<<- y
    inverse<<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cachesolve function takes a matrix created by the makecache matrix
## and calculates its matrix if not already available in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
