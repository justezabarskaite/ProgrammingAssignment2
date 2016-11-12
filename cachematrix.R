# R-programming, Programming Assignment 2
#
## example of execution in console:
## data <- rbind(c(1, -1/5), c(-1/5, 1)) 
## x <- makeCacheMatrix(data)
## cacheSolve(x) ## generated inverse
## cacheSolve(x) ## inverse from cache

## This function creates a special "matrix" object that can cache its inverse.
## Input:
##   x: a matrix object to be inversed
## Output:
##   list of set/get, setinverse/getinverse functions 

makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function () i
    
    ## return list of functions
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Input:
##   x: a list of functions returned by makeCacheMatrix() function
##   ...: other arguments passed to solve() function
## Output:
##   i: a matrix object, inverse matrix of x

cacheSolve <- function(x, ...) {
    
    ## check of inverse already exists in cache, if yes, return cache
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    ## if inverse does not exist in cache, generate inverse and return it
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
