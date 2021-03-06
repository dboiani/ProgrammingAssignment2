## Put comments here that give an overall description of what your
## functions do

## The two functions are used to create a special object that
## stores a matrix and cache's its inverse (solve()).

## Write a short comment describing this function

## creates a special "vector", which is a list containing 
## a function to: 1) set the value of the matrix; 2) get the 
## value of the matrix; 3) set (setinverse) the value of the 
## inverse matrix (solve()); 4) get (getinverse) the value of the 
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x

    ## set (setinverse) the value of the inverse matrix (solve())
    setinverse <- function(solve) i <<- solve

    ## get (getinverse) the value of the inverse matrix
    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Uses vector created with the makeCacheMatrix function.
## First checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matirx from the cache.  Otherwise, it
## calculates the inverse matrix of the matrix and sets the value of the
## inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## check to see if the cache is not null
    if(!is.null(i)) {
        message("getting cached data")
        
        ##return the cached inverse of the matrix
        return(i)
    }
    ## retrieve the original matrix
    data <- x$get()
    
    ## get the inverse of the retrieved matrix
    i <- solve(data, ...)
    
    ## cache the inverse of the retrieved matrix
        x$setinverse(i)
    
    ## print the cached inverse of the retrieved matrix
    i
}
