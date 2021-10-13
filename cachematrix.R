# Matrix inversion is usually a costly computation. There is some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. This 
# script contains a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its 
# inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  cached_inverse <- NULL
  set_inverse <- function(y) cached_inverse <<- y
  get_inverse <- function() cached_inverse
  
  set_original <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get_original <- function() x
  
  list(set_original=set_original,get_original=get_original,
       set_inverse=set_inverse,get_inverse=get_inverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cacheSolve should retrieve the inverse 
# from the cache. It assumes the matrix is square and invertible (e.g. 
# matrix(c(2,2,3,2),2,2). Returns a matrix that is the inverse of 'x'

cacheSolve <- function(cacheMatrix, ...) {
  if (is.null(cacheMatrix$get_inverse())) {
    temp_inverse<-solve(cacheMatrix$get_original(), ...)
    cacheMatrix$set_inverse(temp_inverse)
    return(temp_inverse)
  } else {
    print("Used the cache")
    return(cacheMatrix$get_inverse())
  }
}

m<-makeCacheMatrix(matrix(c(2,2,3,2),2,2))
n<-cacheSolve(m)
m$get_inverse()
m$get_original() %*% n
