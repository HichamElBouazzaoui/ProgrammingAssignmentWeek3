## Put comments here that give an overall description of what your
## functions do

## This function stores a variable or in this case a matrix in the cache environment
## so this it can be accessed fast when calling the matrix in a order function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getmatrix = getmatrix)
  
}


## This function uses the matrix stored in the cache environment and calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
