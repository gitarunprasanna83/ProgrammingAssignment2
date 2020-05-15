## makeCacheMatrix will set the value of matrix, get the value of maxtrix, set the inverse of matrix and gets (cache) its inverse
## cacheSolve will compute the inverse of special matrix returned by makeCacheMatrix. If it is already present in cache, then it retreives it without recomputing it.

## Assign object m to Null, assign x (matrix object) to lexical scoping variable y (used within)
## set m to inverse value of x and store it in m as cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## first retrieve the value of m from previous function. If the value is NOT null, return m else, solve the matrix for inverse of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(mat, ...)
  x$setInv(m)
  m
}
