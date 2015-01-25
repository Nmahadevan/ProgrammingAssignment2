## This file contains two functions
## makeCacheMatrix - this function creates a vector of four functions
## cacheSolve - This function returns the inverse of a matrix

## makeCacheMatrix function creates a vector of four functions
      ## GETMAT - Get the input matrix
      ## SETMAT - assigns input matrix to a variable
      ## SETINVMAT - computes the inverse of a matrix
      ## GETINVMAT - gets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
      
      invmat <- NULL  ## Assigns NULL to variable that will store the inverse of matrix
      setmat <- function(y) {
            x <<- y   
            invmat <<- NULL
      }
      getmat <- function() x  ## returns the matrix
      setinvmat <- function(solve) invmat <<- solve ## calculates inverse of matrix and assigns to invmat
      getinvmat <- function() invmat  ## returns the value of invmat
      list(setmat = setmat, getmat = getmat,
           setinvmat = setinvmat,
           getinvmat = getinvmat)

}


## cacheSolve function returns the inverse of matrix. 
## It first checks if the inverse matrix is already calculated, If available it returns from cache
## else it computes by using Solve function and returns it.It also sets the inverse of matrix to cache for future use.
cacheSolve <- function(x, ...) {
        
      invmat <- x$getinvmat() 
      if(!is.null(invmat)) { ## checks if invmat is blank
            message("getting inverse matrix from cache")
            return(invmat)  ## returns inverse of matrix from cache
      }
      mat <- x$getmat() ## assigns mat with input matrix
      invmat <- solve(mat, ...)  ## calculates inverse of matrix
      x$setinvmat(invmat) ## stores inverse of matrix to cache
      message("calculating inverse matrix")
      invmat
}
