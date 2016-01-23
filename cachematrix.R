## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 


## Function to create a matrix object, this new object caches the
## inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <-  function()x
    setInverse <-  function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function gets a object x created by makeCacheMatrix and returns solve(x)
## It also caches the value of solve(x) to speed up future access
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <-  solve(data,...)
    x$setInverse(inv)
    inv
}


## To test:
##  m1 <- rbind(c(1, -1/4), c(-1/4, 1))
##  sm <-  makeCacheMatrix(m1)
##  invsm <-  cacheSolve(sm)
##  invsm %*%m1  returns a normal diagonal matrix
##  Call invsm <-  cacheSolve(sm) prints message 'getting cached data'


