## Next functions are used to make use of the scoping rules in R to cache
## a value that is potentially time-consuming to calculate, and that may be 
## needed to do some other calculations. In this case the value is the inverse
## of a square invertible matrix.

## The first function creates a special matrix that is really a list
## containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the value of the matrix inverse to NULL
    minv <- NULL                     
    ## declare another function set where minv will be cached 
    set <- function(y) {                      
        x <<- y
        ## change the value of minv in case the matrix was changed.
        minv <<- NULL              
    }
    ## gets the value of the inverse
    get <- function() x                           
    #calculates the inverse an invertible matrix via the solve function
    setinverse <- function(solve) minv <<- solve 
    # gets the inverse     
    getinverse <- function() minv        
    ## returns the value of the function makeCacheMatrix        
    list(set = set, get = get,                    
         setinverse = setinverse,
         getinverse = getinverse)
}

## Next function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    #if the inverse exists, it gets it.
    if(!is.null(minv)) {                 
        message("getting cached data - Inverse of the matrix")
        return(minv)
    }
    #if the inverse if not in cache, it is calculated and then returned
    data <- x$get()                               
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
