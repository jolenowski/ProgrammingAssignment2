
# creates a list containt a function to do the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) { 
    
    x <<- y
    inv <<- NUll
  }
  get <- function () x 
  setinverse <-function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inv of the matrix. Checks to see if its computed and if so returns it 
## if not it it computes it

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("geting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
