
## This function creates a special "matrix" object that can cache its inverse
##(checks the matrix for degeneracy and squareness)

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x)!=ncol(x)){
    return("Matrix is a non-square and does not have an inverse")
  }
  else if (det(x)==0) {
    return("Matrix is a degenerate and does not have an inverse")
  }
  else {
    c <- NULL
    set <- function(y) {
      x <<- y
      c <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) c <<- solve
    getsolve <- function() c
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  c <- x$getsolve()
  if(!is.null(c)) {
    message("getting inversed matrix")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setsolve(c)
  c
}


