# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<-solve
  getInv <-function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix().
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve() should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}