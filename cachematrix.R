## Stores the inverse of a matrix in a "special matrix" 
## If available in the "special matrix" returns the stored inverse matrix, otherwise, calculates de inverse, stores it
## in the "special vector" and returns it value

## Creates a "special matrix" that stores the inverse of the matrix "x"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If the "special matrix" "x" contains its inverse matrix, returns the value.Otherwise, calculates the inverse
## of the "special matrix" and returns its value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}  
