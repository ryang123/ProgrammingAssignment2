makeCacheMatrix <- function(x = matrix()) {
  ## Set initial value to NULL
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## Return matrix when get is called
  get <- function() x
  ## Define the solve function
  setsolve <- function(solve) s <<- solve
  ## Define the get function for the solved value
  getsolve <- function() s
  ## List the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ## Try to retrieve solved value, if already solved
  s <- x$getsolve()
  ## If retrieved value not NULL, return that and do not proceed
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## If retrieved value was NULL, we get the matric and solve
  data <- x$get()
  s <- solve(data, ...)
  ## Set the solve value in cache
  x$setsolve(s)
  ## Return solved value
  s
}