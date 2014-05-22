## These functions create a special matrix that can be cached and retrieved to lower computational "overhead"
## The first creates the special matrix and the second computes the inverse of the matrix or retrieves the cached value


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  x<-matrix(x,nrow=sqrt(length(x)))
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- solve(x)%*%x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- matrix(data, ...)
  x$setmatrix(m)

}
