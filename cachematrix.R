## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly
## This assumes that the matrix supplied is a square matrix that is always invertible. 
## Computing the inverse of a square matrix was done with the solve function in R

## The following functions are used to create a special object that stores a matrix vector 
## and cache's its inverse.


## The makeCacheMatrix is used to create a special list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) m <<- solve

        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special matrix 

cacheSolve <- function(x, ...) {
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

# Reference: this was based on the catchment and catchsolve from the week 2 
# programming assignment,R Programming by John Hopkins University 

# You may enter the following in the command line to test the above functions
#> test <- matrix( 
#   c(2, 4, 3, 1, 5, 7, 3, 4, 6), # the data elements 
#   nrow=3,              # number of rows 
#    ncol=3,              # number of columns 
#   byrow = TRUE)
# 
#> x <- makeCacheMatrix(test)
#> y <- cacheSolve(x)


