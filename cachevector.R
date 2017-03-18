#Progrmming assignment2: R Programming Coursera

## These two functions are used to compute an inverse of a matrix if
## it has not computed already. If it has computed before, it will retrieve from cache
## otherwise it will compute the inverse of the matrix.

## This function creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverse from the cached matrix value that is stored in the makeCacheMatrix() object's 
## environment.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
