## Caching the Inverse of a Matrix to reduce the need of recalculating

## Goal of makeCacheMatrix: "This function creates a special "matrix" object that can cache its inverse."

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y #
    m <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the value of the inverse
  setsolve <- function(solve) m <<- solve
  ## get the value of the inverse
  getsolve <- function() m
  ##return the list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##  Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## it first checks to see if the inverse has already been calculated. 
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## If so, it `get`s the inverse from the cache and skips the computation. 
  }
  data <- x$get() ## Otherwise, it calculates the inverse of the matrix 
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#Check to see if the functions work correctly (obtained from the Course Week 3 Discussion Forum)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
solve(m1)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object) # should return same result as solve(m1)

# you can use the set function to "put in" a new matrix.
# For example n2
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
# and obtain its matrix inverse by
cacheSolve(myMatrix_object)