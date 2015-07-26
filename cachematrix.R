## This module contains the following functions 
##  makeCacheMatrix(x) - Creates a matrix object that can cache its inverse.
##  cacheSolve(x)      - Computes inverse of a matrix and caches the computed value for future re-use.


# This function converts a matrix to an object capable of storing its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <-NULL
  
  set<-function(y) {
    
    x <<- y 
    inv <<- NULL
    
  }
  
  get <- function() x 
  
  setInv<-function(inverse) inv <<-inverse
  
  getInv<-function() inv 
  
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}



# Computes inverse of a matrix x and caches its value for future re-use. Must call makeCacheMatrix 
# on standard R matrix to convert to input to this routine.  
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

  # check if inverse of matrix x is already cached.
  inv <- x$getInv()
  
  # if cached inverse of x found then return it. Else compute and cache for future use.
  if (!is.null(inv)) {
    
      message("getting cached data")
      return (inv)
    
  }

  # Get a cached copy of x
  data<-x$get() 
  
  #compute the inverse of x.
  inv<-solve(data)
  
  # cache the inverse of x
  x$setInv(inv)
  
  return(inv)
}
