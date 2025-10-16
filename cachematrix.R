## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix method creates a matrix object with the following funtions
#   1. Set the value of the matrix
#   2. Get the value of the matrix
#   3. Set the value of the matrix inverse
#   4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The cacheSolve method returns the inverse of a matrix object if it has been assigned to the object.  If the 
# inverse has not been calculated/defined, the function will calculate and set the inverse matrix and store it
# the matrix object.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)){
          message("retrieving inverse matrix")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      return(inv)
}
