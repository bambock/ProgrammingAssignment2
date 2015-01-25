## Ethan Bambock
## R-Programming - Assignment 2
## 2015/01/22

## This script provides a reuseable method for setting and getting a matrix and it's inverse values.  
## Additionally, the cached data can be retained using a global variable ("i") that doesn't have to
## recalculated when the function getinverse() is called.  This technique allows an application or process
## to run more efficiently without having to process unnecessary work.

## This function is used to define a special 'matrix' object with a series of functions that can be 
## called to obtain or set a matrix value and cache it's inverse.  

makeCacheMatrix <- function(mx = matrix()) {
    
    message("makecachematrix: initializing matrix object and functions")
    # initializing the inverse matrix object to NULL
    i <- NULL
    
    # set() is used to store matrix passed as an argument or sets a default
    set <- function(y) {
        mx <<- y
        i <<- NULL
    }
    
    # get() returns the value of the matrix that has been stored
    get <- function() mx
    
    # setinverse() is used to store the inverse matrix 'globally' for the currently set matrix
    setinverse <- function(inverse) i <<- inverse
    
    # getinverse() returns the inverse matrix that is currently stored 'globally'
    getinverse <- function() i
    
    # last command evaluated and returns a list of the defined functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is used to verify if a matrix inverse has been set and use the data stored.  If
## the inverse matrix returned from getmatrix() above is NULL, the data is obtained and the inverse
## is calculated with the solve() function, stored, and returned.

cacheSolve <- function(mx, ...) {
    ## Return a matrix that is the inverse of 'mx'
    i <- mx$getinverse()
    
    # if the data returned is not null, return the value retrieved from getinverse()
    if(!is.null(i)) {
        message("cachesolve: getting cached inverse matrix")
        return(i)
    }
    
    # otherwise cache the new data and return the value
    message("cachesolve: setting cached inverse matrix")
    data <- mx$get()
    i <- solve(data, ...)
    mx$setinverse(i)
    i
}