This function creates a matrix which is able to cache its inverse.
This way it is not necessary to recompute it everytime we need it.
First, we set and get the values of the special matrix. Later, we set 
and get the values of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


This function checks if the inverse computation has been done, 
avoiding recomputing again and returning it. If not, it computes 
the inverse matrix of the earlier function with the solve() function, 
and sets it in the cache. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}
