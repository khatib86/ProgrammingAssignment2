makeCacheMatrix <- function(x = matrix()) 
{ 
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_mat) m <<- inv_mat
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
} 


## This function creates the cacheSolve function which will retrive the inverse of the 
##non singular matrix whose determinate in not zero. If matrix will exist then it will fetch it from cache

cacheSolve <- function(x, ...) 
{ 
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getinverse()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} 
