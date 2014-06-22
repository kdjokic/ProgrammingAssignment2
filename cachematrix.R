## Two functions for:
## a) creating matrix object for caching its inverse
## b) computing the inverse of the matrix or pulling from cache

## Create matrix object for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse<-NULL
  set <- function(y) { ##setting matrix
        x <<- y
        matrix_inverse <<- NULL
  }
  get <- function() x ##getting matrix
  set_inverse <- function(minv) matrix_inverse <<- minv ##setting inverse matrix
  get_inverse <- function() matrix_inverse ##getting inverse matrix
  list(set = set, get = get, set_inverse=set_inverse, get_inverse=get_inverse) ##actions with matrix object
}


## Computes the inverse of the matrix if it can't be found in cache, othervise
## it will be pull from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$get_inverse()
  if(!is.null(matrix_inverse)) { ##if it can be found in cache than return it
    message("getting cached matrix inverse")
    return(matrix_inverse)
  }
  ##if it cannot be found in a cache than calculate it and return it
  data <- x$get() 
  matrix_inverse <- solve(data, ...)
  x$set_inverse(matrix_inverse)
  matrix_inverse
}
