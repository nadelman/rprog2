## Implemenation of caching facade over the core solve function from 
## the base R package
## 

## 
# Facade over a the matrix x, providing a cached version of the matrix's inverse
#
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function() {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
      x 
  }
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
    inv
  }
  
  # return a list of these functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##
# Caching facade over the solve method from the bast R package.
# 
# Assumes that x is of the form defined by makeCacheMatrix. If the inverse of the matrix
# x has already been calculated, the cached version will be returned. Otherwise, will
# delegate to the solve method from the core R packages to obtain the result. Once the 
# inverse is obtained, it will be cached for the next time that the inverse for this
# matrix is requested.
##
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  ## return 
  inv
}
