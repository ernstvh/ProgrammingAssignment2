## This file contains two functions that, together, cache the inverse of a matrix:
## 'makeCacheMatrix' a constructor function for a special matrix object that can calculate and cache its inverse
## 'cacheSolve' a solve function to calculate and cache a matrix's inverse, it uses 'makeCacheMatrix' to do the heavy lifting

## Constructor function for a special matrix object that caches its inverse 
## 'x' is the matrix for which the inverse will be cached
## Use '$set' and '$get' to update and access 'x'
## Use '$solve_inverse' to calculate or access the cached inverse
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
    
    ## Returns the inverse from cache when there is a cached value
    ## Otherwise, the inverse will be calculated and cached first, before it is returned 
    solve_inverse <- function() {
        if (is.null(cached_inverse)) {
            ## cache-miss -- calculate and cache the inverse
            cached_inverse <<- solve(get())
        }
        else {
            ## cache-hit
            message("Returning cached inverse")
        }
        
        ## Return a matrix that is the inverse of 'a'
        cached_inverse
    }
    
    list(
        set=set, 
        get=get,
        solve_inverse=solve_inverse
    )
}

## A function that calculates and caches the inverse of a matrix 'a'.
## When the inverse of 'a' is not in cache (cache miss), its value is calculated, cached and returned
## When the inverse of 'a' is in cache (cache hit), the value from cache is returned.
## In case also a 'b' is given, the ordinary solve function is used and no caching takes place
## Note: 'a' is assumed to be constructed with 'makeCacheMatrix'
cacheSolve <- function(a, b=NULL, ...) {
    
    if (!is.null(b)) {
        # 'b' is given: just return the result of solve
        solve(a$get(), b, ...)
    }
    else {
        a$solve_inverse()
    }
}

## Example matrices
a <- matrix(c(1,4,2,3), 2, 2)
b <- matrix(sample(1:1000000, 1000000), 1000, 1000)



