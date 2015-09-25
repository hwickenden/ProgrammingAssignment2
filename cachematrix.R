# This is a submitted solution for the 2nd Programming Assignment of "R Programming".
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# These functions assume that the matrix supplied is always invertible.

# Part 1
# makeCacheMatrix: This function creates a special "matrix" object that can cache
#                  its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set_matrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get_matrix <- function() x
  set_inverse <- function(solve) inv <<- solve
  get_inverse <- function() inv
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# Part 2
# cacheSolve:This function computes the inverse of the special "matrix" returned by 
#            makeCacheMatrix above. If the inverse has already been calculated (and
#            the matrix has not changed), then the cachesolve should retrieve the
#            inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get_matrix()
  inv <- solve(mat, ...)
  x$set_inverse(inv)
  inv
}

