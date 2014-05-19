# Defines a type of matrix that can be used to cache the value of it's inverse.
# The makeCacheMatrix function creates a CacheMatrix
# The cacheSolve function can be used to calculate their inverse, 
# returning the cached value if already calculated


# Creates a special "matrix" with the ability fo cache it's inverse
# when used together with the cacheSolve function.
# The function returns a list containting the following functions:
#   - x$get() : return the matrix
#   - x$set(y) : set the value of the matrix
#   - x$getinverse() : get the cached value of the inverse (
#                      will reutrn NULL if not already calculated)
#   - x$setinverse(y) : set the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




# Returns a matrix that is the inverse of x, where x is a "CacheMatrix"
# created using the makeCacheMatrix function
# If the inverse of this matrix has already been calculated, the 
# cached value is used, otherwise the "solve" function is used
# to calculate the inverse and the value is cached.
# Additional arguments specified with ... are passed to solve
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) {
    inv <- solve(x$get(), ...)
    x$setinv(inv)
  }
  inv
}
