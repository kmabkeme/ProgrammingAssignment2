## The pair of functions makeCacheMatrix() and cacheSolve() provide a way
## to store the values of a matrix and its inverse (the product of a matrix
## and its inverse is the identity matrix, with 1's along the diagonal and
## zeroes elsewhere) and retrieve them, in such a fashion that the inverse
## needs to be computed only once, and it can subsequently be retrieved from
## the stored value in the makeCacheMatrix() environment.

## makeCacheMatrix() takes as input an invertible matrix of any dimension
## The values in the matrix get stored in the environment
## instantiated within makeCacheMatrix(). This environment can also contain
## the value of the inverse of the input matrix. The original matrix may be
## stored and retrieved from this environment via set() and get(), respectively.
## The inverse matrix may be stored and retrieved from this environment
## via setInverse() and getInverse(), respectively.

makeCacheMatrix <- function(x = matrix()) {  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
    }
  get <- function() x
  setInverse <- function(inverseM) inverseMatrix <<- inverseM
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## cacheSolve() takes the list of values put out by makeCacheMatrix()
## and uses that interface of get(), setInverse(), and getInverse() to check
## whether there is already a value calculated for the inverse of the matrix 
## entered into makeCacheMatrix(). If the inverse matrix value is NULL,
## then the matrix method solve() gets called, and the inverse is calculated and
## gets stored into the makeCacheMatrix() environment. If the inverse matrix
## has already been calculated before and set in the makeCacheMatrix()
## environment, then the stored value is retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
