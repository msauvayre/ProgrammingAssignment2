## This is my solution of makeCacheMatrix
## This function have four methods in her :
## Set & Get a matrix
## Set & Get the invert of the created matrix

makeCacheMatrix <- function(my_mat = matrix()) {
      inv_mat <- NULL
      # First method : set a matrix
	  set <- function(y) {
            my_mat <<- y
            inv_mat <<- NULL
      }
	  # Second method : get the created matrix
      get <- function() my_mat
	  # Third & fourth method : set & get the inverted matrix previously created
      setinvert <- function(solve) inv_mat <<- solve
      getinvert <- function() inv_mat
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}

## This is my solution of CacheSolve, a function who give the invert of a matrix
## who was created before with makeCacheMatrix. It check if the inverted matrix
## exist, if so, it compute the inverse of this

cacheSolve <- function(inv_mat, ...) {
      ## Return a matrix that is the inverse of 'inv_mat'
      my_mat <- inv_mat$getinvert()
	  # Test if the inverted matrix previously created exist
      if(!is.null(my_mat)) {
            message("getting invert data")
            return(my_mat)
      }
      data <- inv_mat$get()
      my_mat <- solve(data, ...)
      inv_mat$setinvert(my_mat)
      my_mat
}
