##The functions to cache and calculate the inverse of a matrix

## Creates a "matrix" object that can used to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() inv
  
  ## Return a list of the methods
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##It computes the inverse of the special matrix returned by "makeCacheMatrix".
##In case, if the inverse matrix has already been calculated this function
##retrieves the inverse one from the cache and skips the computation.
##If not, it calculates the inverse of the matrix and put it in the cache using 
##the "setinverse" function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  cur_matrix <- x$get()
  
  inv <- solve(cur_matrix, ...)
  
  x$setinverse(inv)
  inv
}
