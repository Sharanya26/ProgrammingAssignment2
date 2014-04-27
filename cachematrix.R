#### Caching the Inverse of a Matrix
## The below functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  cachedMatrix <- NULL  
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) cachedMatrix <<- solve
  getInverse <- function() cachedMatrix
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {  
  
  cachedMatrix <- x$getInverse()
  if(!is.null(cachedMatrix)) {
    message("getting cached data")  
    return(cachedMatrix)            
  }
  
  data <- x$get()
  cachedMatrix <- solve(data, ...)
  x$setInverse(cachedMatrix)
  cachedMatrix                      ## This returns a matrix that is the inverse of 'x'
}
