## I started out by creating an empty object/skeleton which I then used
##to assign the values of both the matrix that I created and the
##inverse that I eventually solved for.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Once the skeleton was created (as above), the following lines then
##proceed to call the inverse from the cache (if it had already been created before)
##or compute a new one in its absence.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}  ## Return a matrix that is the inverse of 'x'

