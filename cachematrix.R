# These functions help create matrices that can cache its inverses
# I'll be using TOMDOC (http://tomdoc.org/) to document them

# Public: Creates a matrix capable of storing its inverse
#
# x  - The original matrix
#
# Examples
#
#   makeCacheMatrix(replicate(1000, rnorm(1000)))
#
# Returns the new matrix with superpowers
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
          x <<- y
          s <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) s <<- inverse
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Public: Calculates the inverse and caches it, returning the cache
# if it was already available
#
# x  - A matrix created with makeCacheMatrix
#
# Returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
