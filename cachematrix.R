## Put comments here that give an overall description of what your
## functions do

## These functions allow to alleviate some computation-intensive calculations on a matrix inversion in the case
## a matrix has already been inverted. We will allow it to cache the inverted matrix for later retrieval, and 
## include functionality to get and set both the matrix itself and its inverse.


## Write a short comment describing this function
## This function will create a list containing functions to get and set both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## on the Set, simply assign the matrix to itself, and let NULL be the inverse as it has not been calculated yet
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        
        ## Get will return the matrix itself simply
        get <- function() x
        
        ## SetInverse will assign the inverse to the 'inv'
        SetInverse <- function(solve) inv <<- solve
        
        ## GetInverse will return the inverse
        GetInverse <- function() inv
        
        ## Create the list associated with this function
        list(set = set, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the Matrix and caches it - if it has already been calculated,
## the function will return the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$GetInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$SetInverse(inv)
        inv
}
