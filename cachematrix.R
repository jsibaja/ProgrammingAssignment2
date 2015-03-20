## Notes: makecacheMatrix function, creates a special matrix object with functions 
##        'get','set' for the matrix values. Also, to set and get the inversion
##        matrix from the cache area.
##        
##        cacheSolve function returns the inverse of the matrix from the cache if
##        this has been calculated, otherwise will compute the Inverse matrix.
## 03/20/2015 Jose Sibaja


## Create a Matrix Object and a list of functions to set and get the inverse
## matrix and save it to the cache.

makeCacheMatrix <- function(x = matrix()) {
      
      inverseMatrix <- NULL
      
      set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
      }
      
      get <- function() {
        x
      }
      ## Compute the inverse
      setinverse <- function(solve) {
        inverseMatrix <<- solve
      }
      getinverse <- function() {
        inverseMatrix
      }
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getinverse()
      if(!is.null(inverseMatrix)) {
        message("Getting cached data..")
        return(inverseMatrix)
      }
      matrix <- x$get()
      inverseMatrix <- solve(matrix, ...)
      x$setinverse(inverseMatrix)
      inverseMatrix
}
