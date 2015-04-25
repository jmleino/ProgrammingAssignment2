## The functions allow creating a special matrix that can contain
## also its inverse. The inverse is stored in cache so that it needs 
## to be calculated only on the first call.


## makeCacheMatrix creates a special matrix that can cache its inverse.
## It allows setting and getting the value of the matrix and the inverse 
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      invx <- NULL

      #Function to set the value of the matrix
      set <- function(y) {
            x <<- y
            invx <<- NULL
      }

      #Function to get the value of the matrix
      get <- function() x

      #Function to set the value of the inverse matrix
      setinv <- function(inv) invx <<- inv
        
      #Function to get the value of the inverse matrix
      getinv <- function() invx
        
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of matrix x. 
## The inverse is calculated only on the first call 
## and cached inverse is used afterwards.

cacheSolve <- function(x, ...) {
        
      #Return the inverse if its already cached		
      invx <- x$getinv()
      if(!is.null(invx)) {
            message("getting cached data")
            return(invx)
      }
      #Otherwise solve and store the inverse
      data <- x$get()
      invx <- solve(data, ...)
      x$setinv(invx)
      invx
}
