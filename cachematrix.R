## These functions are designed to find the inverse of a matrix.
## The first function make a special cached matrix and calculates its inverse.
## The second function calculates the inverse of the cached matrix, or extracts the inverse if it has already been calculated.


## This function is designed to create a cached version of the matrix, calculate its inverse and cache it

makeCacheMatrix <- function(x = matrix()) {
     
     inverse_matrix <- NULL  ## sets inverse matrix to NULL
     
     set_matrix <- function(y) {
          x <<- y                  ## changes x to some new matrix y
          inverse_matrix <<- NULL  ## resets the inverse matrix to NULL
     }
     
     get_matrix <- function() x    ## fetches and returns the matrix
     set_inverse <- function(inverse) inverse_matrix <<- inverse ##sets the inverse
     get_inverse <- function() inverse_matrix ##fetches the inverse matrix
     
     list(set_matrix=set_matrix,
          get_matrix=get_matrix,
          set_inverse=set_inverse,
          get_inverse=get_inverse) ## creates our new list of functions
     
}


## this function checks if the inverse matrix has been cached
## If it has, it returns it. Otherwise it calculates and caches it

cacheSolve <- function(z, ...) {
     
     inverse_matrix <- z$get_inverse() ## fetches cached inverse
     if(!is.null(inverse_matrix)) {    ## checks if inverse is not NULL
          message("getting cached inverse")
          return(inverse_matrix)       ## returns inverse if not NULL
     }
     
     original_matrix <- z$get_matrix()   ## recalls cached matrix
     inverse_matrix <- solve(original_matrix) ## solves and creates the inverse
     z$set_inverse(inverse_matrix) ## sets as the cached inverse
     inverse_matrix ## returns the inverse
     
     
}
