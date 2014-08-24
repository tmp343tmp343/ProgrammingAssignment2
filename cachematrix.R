## Programming Assignment 2, 24 August 2014
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions cache the inverse of a matrix, assuming the
## matrix supplied is always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function.
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        } 
        data <- x$get() 
        m <- solve(data, ...) 
        x$setinverse(m) 
        m
}
