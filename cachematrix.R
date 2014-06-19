## These two functions were made to cache the
## value of the Inverse matrix so that when
## we need it again, it can be looked up in 
## the cache rather than recomputed.


## The first function is to create a list that
## contains set matrix, get matrix, set the 
## inverse matrix, and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                   
        set <- function(y) {                 ## set the value of matrix
              x <<- y
              m <<- NULL
        }        
        get <- function() x                  ## get the value of matrix
        setinv <- function(inv) m <<- inv    ## set the value of inverse matrix  
        getinv <- function() m               ## get the value of inverse matrix    
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## calculates the inverse matrix of the 
## list created with the above function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }                                     ## Check if the inverse matrix has been calculated  
        data <- x$get()
        m <- solve(data, ...)                 ## Calculate the inverse matrix   
        x$setinv(m)
        m                                     ## Return a matrix that is the inverse of 'x'  
}