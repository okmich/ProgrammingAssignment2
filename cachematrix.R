## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
