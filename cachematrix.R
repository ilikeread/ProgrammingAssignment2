## Put comments here that give an overall description of what your
## functions do
## 
## Matrix inversion is usually a costly computation and it is benefitial
## to caching the inversion of a matrix rather than compute it repeatedly.
## Two functions below: makeCacheMatrix and cacheSolve
## 
## Write a short comment describing this function
## First function return a list of function include set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## Second function check if the inversion is already available and return it
## or else it compute the inversion if it is not available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting inversion matrix")
        return(inv)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
    
}
