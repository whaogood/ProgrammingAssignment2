## The purpose of the following functions is to cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL    
  }
  get <- function() x
  setim <- function(invmat) im <<- invmat
  getim <- function() im
  ## Make a list containing the four functions to be called by the function of cacheSolve
  list(set = set, get = get, setim = setim, getim = getim)  
}



## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  ## If the inverse has already been calculated (and the matrix) has not changed), then the cachesolve should 
  ## retrieve the inverse from the cache.
  im <- x$getim()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  ## If the inverse has NOT been calculated, the a new calculation will be carried on using solve()
  data <- x$get()
  im <- solve(data)
  x$setim(im)
  im
}
