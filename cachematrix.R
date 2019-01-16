## Put comments here that give an overall description of what your

## We start here

## This function creates a vectpr that set the value of the vector and get it and get the value of the inverse and set it


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set the value of the vector
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the vector
        get <- function() x
        ## set the value of the inverse of the matrix
        setinv <-function (solve) inv <<- solve
        ## get the value of the inverse of the matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This calculates the inverse of the matrix but first checks whether it has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        
}
