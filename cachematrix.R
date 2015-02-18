## Put comments here that give an overall description of what your
## functions do
##  
## The functions in the file provide functionality that can create a matrix which
## which caches its inverse (assuming that it has one) such that without changing
## without changing the values of the matrix, a call to cacheSolve which lookup
## a previously calculated inverse rather than recalculate
##
## Write a short comment describing this function

## makeCacheMatrix creates and encapsulates a internal matrix and returns a set
## list of operations or variables of that list that are
## themselves functions to lookup the interval matrix
makeCacheMatrix <- function(x = matrix()) {
    #store a local copy of the matrix cache and its dimension
    minv <- NULL
    
    #get the cached matrix
    get <- function() x
    
    #set the cached matrix
    set <- function(mat) {
        x <<- mat
        minv <<- NULL
    }
    
    #function returns the cached matrix
    getInverse <- function(mat) minv
        
    #function to set the cache matrix 
    setInverse <- function(inv){
        minv <<- inv
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

## Write a short comment describing this function
## cacheSolves accepts a data structure from the function
## makeCacheMatrix as x and returns the inverse of it.
## it first checks to see if there already exist an inverse 
## If such inverse exist, it returns it else it calculates the
## the inverse of the matrixs and assigns it back to x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    #get cached matrix
    matr <- x$get()
    
    if (!is.null(inv)){
        return(inv)
    }
    inverse1 <- solve(matr)
    x$setInverse(inverse1)
    
    inverse1
}
