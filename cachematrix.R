## makeCacheMatrix creates a matrix object. It's inverse is calculated
## by cacheSolve.
## If the inverse has already been calcuated, it will retrieve it from
## the cache and return it. Else, it will calculate the inverse and
## set it and return it. 


## makeCacheMatrix creates a special matrix object that can cache
## it's inverse 

## @x           is a square invertible matrix
## @return      a list containing functions to 
##                      1)      set the value of the matrix
##                      2)      get the value of the matrix
##                      3)      set the value of the matrix inverse
##                      4)      get the value of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL 
        set <- function(y)
        {
                x<<- y 
                inverse<- NULL
        }
        
        get <- function() x 
        setinv <- function(inv) inverse <<- inverse
        getinv <- function() inverse 
        list(set = set , get = get, setinv = setinv, getinv = getinv)

}


## Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has
## not changed), then cacheSolve should retrieve the inverse from
## the cache. 

## @x           return from makeCacheMatrix()
## @return      return the inverse of matrix         
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse))
        {
                message("getting cached data... ")
                return(inverse)
        }
        else
        {
                ## calculate inverse of matrix
                inverse <- solve(x$get()) 
                ## set value of inverse in cache
                x$setinv(inverse)
                
                return(inverse)
        }
                
}
