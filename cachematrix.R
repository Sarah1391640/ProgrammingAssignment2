
## create special object of "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculate the inverse of the special matrix created with the above function
## check if the inverse has been calculated. if so, get the inverse from the cache
## if not, calculate the inverse of the special matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Testing

funs <- makeCacheMatrix()
funs$set(matrix(1:4, 2))
funs$get()
funs$setInverse()
funs$getInverse()
cacheSolve(funs)


