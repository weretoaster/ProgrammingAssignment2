## These two functions compute, cache and return the inverse of a matrix 'x':

## 1. create scope with data and functions that will be called by cacheSolve to compute the inverse of a matrix 'x'

makeCacheMatrix <- function(x = matrix()) { ## function that returns a list of functions that operate on x
    
    inv <- NULL ## initialise inv as NULL
    
    set <- function(y) { ## function that can set a new value for x if needed
        x <<- y ## replace x with y
        inv <<- NULL ## discard cached inverse of x
    }
    get <- function() x ## function that gets x
    setinv <- function(newinv) inv <<- newinv ## function that caches the inverse of x
    getinv <- function() inv ## function that returns a value for inv (cached data for the inverse of x if it exists, otherwise NULL)
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv) ## list of the above function that operate on x
    
}

## 2. check if the inverse of x has been cached and, if not, compute its inverse

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv() ## get the value for inv (the cached data for x if it exists, otherwise NULL)
    if(!is.null(inv)) { ## check to see whether cached data exists for x and, if so, return it
        message("getting cached data") ## alert the user that cached data for x is being returned
        return(inv) ## return the cached data for x
    } 
    
    ## if cached data for x is not found i.e. inv is NULL:
    
    data <- x$get() ## get x
    inv <- solve(data, ...) ## compute the inverse of x
    x$setinv(inv) ## cache the inverse of x
    inv ## return a matrix that is the inverse of 'x'
}