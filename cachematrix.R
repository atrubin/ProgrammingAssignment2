# Create object with functions:
# get() - returns original matrix
# set() - set matrix 
# getSolve() - return cached data or NULL
# setSolve() - save cached data
#

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Function checks whether object (x) has already cached data (s) 
#  If there is a cached data (s is not NULL), return it without computation
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
