## Put comments here that give an overall description of what your
## functions do
## test hash
## Write a short comment describing this function

makeCacheMatrix <- function(my_mat = matrix()) {
      inv_mat <- NULL
      set <- function(y) {
            my_mat <<- y
            inv_mat <<- NULL
      }
      get <- function() my_mat
      setinvert <- function(solve) inv_mat <<- solve
      getinvert <- function() inv_mat
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(inv_mat, ...) {
        ## Return a matrix that is the inverse of 'x'
      my_mat <- inv_mat$getinvert()
      if(!is.null(my_mat)) {
            message("getting invert data")
            return(my_mat)
      }
      data <- inv_mat$get()
      my_mat <- solve(data, ...)
      inv_mat$setinvert(my_mat)
      my_mat
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
test <- cacheSolve(m)
print(test)
cacheSolve(test)
