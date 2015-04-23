## There are two functions that allow to calculate the inverse of a matrix if the matrix has been cached before. 
## The inverse of a matrix will be returned from the cache rather than recomputed.

## The makeCacheMatrix function creates a list or a cpecial "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    #set the value of the matrix
  setinverse <- function(inverse) m <<- inverse    #get the value of the matrix
  getinverse <- function() m    #set the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    #get the inverse of the matrix
}


## The cacheSolve function checks if the inverse of the matrix has been calculated, 
## then gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
