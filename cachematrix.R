## This file contains two functions for caching the inverse of a matrix:
## 'makeCacheMatrix' a constructor function for a special matrix object that can cache its inverse
## 'cacheSolve' a solve function to calculate and cache a matrix's inverse

## Constructor function for a special matrix object that caches its inverse 
## 'x' is the matrix for which the inverse will be cached
## Use '$set' and '$get' to update and access 'x'
## Use '$set_inverse'  and '$get_inverse'  to update and access the cached inverse
## Note: changing 'x' will clear the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL

    ## Set the matrix to 'new_x' and clear the cached inverse
    set <- function(new_x) {
        x <<- new_x
        cached_inverse <<- NULL
    }

    ## Get the previously set matrix
    get <- function() {
        x
    }
    
    ## Set the cached inverse to 'inverse' 
    set_inverse <- function(inverse) {
        cached_inverse <<- inverse
    }
    
    ## Get the cached inverse
    get_inverse <- function() {
        cached_inverse
    }
    
    list(
        set=set, 
        get=get,
        set_inverse=set_inverse,
        get_inverse=get_inverse
    )
}

## A function that calculates and caches the inverse of a matrix 'x'.
## on a cache miss ('x' is not in cache), the value is calculated, cached and returned
## on a cache hit ('x' is in cache), the value from cache is returned.
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    
    if (is.null(inverse)) {
        ## cache-miss -- calculate and cache the inverse
        inverse <- solve(x$get())
        x$set_inverse(inverse, ...)
    }
    else {
        message("Returning cached inverse")
    }
        
    ## Return a matrix that is the inverse of 'x'
    inverse
}

## Example matrices
a <- matrix(c(1,4,2,3), 2, 2)
b <- matrix(sample(1:1000000, 1000000), 1000, 1000)



