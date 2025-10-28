## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  # s holds the cached inverse; it's NULL when nothing is cached
  s <- NULL 
  
  # set: re-sets the matrix and clears the old cache
  set <- function(y) { 
    x <<- y
    s <<- NULL # Clears the cache when a new matrix is set
  }
  
  # get: returns the matrix
  get <- function() x 
  
  # setsolve: stores the calculated inverse in the cache (s)
  setsolve <- function(solve) s <<- solve 
  
  # getsolve: returns the cached inverse (s)
  getsolve <- function() s 
  
  # Returns a list of these functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Try to get the cached inverse
  s <- x$getsolve() 
  
  # If the cache is not NULL, return the cached inverse
  if(!is.null(s)) { 
    message("getting inversed matrix")
    return(s)
  }
  
  # If cache was NULL:
  # 1. Get the matrix
  data <- x$get() 
  # 2. Calculate the inverse
  s <- solve(data, ...) 
  # 3. Store the inverse in the cache
  x$setsolve(s) 
  # 4. Return the newly calculated inverse
  s 
}
