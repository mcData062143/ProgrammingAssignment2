
## The following 2 functions calculate the inverse of a matrix after 
## checking to see if the inverted value has already been calculated. 

## The function below creates other functions that set and get the value of
## a matrix, and set and get values of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(solve) m <<- solve
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

## This function calculates the inverse of the matrix after checking to 
## see if it's already been calculated.  If previously calculated, it uses
## that value.  Otherwise it calcuates the inverse and sets the result in cache.

cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m     
}
