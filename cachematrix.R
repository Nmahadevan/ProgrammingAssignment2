## This file contains two functions
## makeCacheMatrix - this function creates a vector of four functions
## cacheSolve - This function returns the inverse of a matrix

## makeCacheMatrix function creates a vector of four functions
      ## GETMAT - Get the input matrix
      ## SETMAT - assigns input matrix to a variable
      ## SETINVMAT - computes the inverse of a matrix
      ## GETINVMAT - gets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
      
      invmat <- NULL
      setmat <- function(y) {
            x <<- y
            invmat <<- NULL
      }
      getmat <- function() x
      setinvmat <- function(solve) invmat <<- solve
      getinvmat <- function() invmat
      list(setmat = setmat, getmat = getmat,
           setinvmat = setinvmat,
           getinvmat = getinvmat)

}


## cacheSolve function returns the inverse of matrix. 
## It first checks if the inverse matrix is already calculated, If available it returns from cache
## else it computes by using Solve function and returns it.It also sets the inverse of matrix to cache for future use.
cacheSolve <- function(x, ...) {
        
      invmat <- x$getinvmat()
      if(!is.null(invmat)) {
            message("getting inverse matrix from cache")
            return(invmat)
      }
      mat <- x$getmat()
      invmat <- solve(mat, ...)
      x$setinvmat(invmat)
      message("calculating inverse matrix")
      invmat
}
