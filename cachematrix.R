## Creates a matrix and calculates the inverse to store in cache.
## Returns cached matrix or calculates a new one.

## Makes a matrix object and caches the inverse
  

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y){
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) xinv <<- solve
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Checks for or creates inverse matrix of "x", returns inverse of "x"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse
  if (!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data <- x$get
  xinv <- solve(data)
  x$setinverse(xinv)
  xinv
}

