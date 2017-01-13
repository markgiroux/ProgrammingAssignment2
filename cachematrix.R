## To Run :
## 1. x <- matrix(1:4, 2, 2)
## 2. makeCacheMatrix(x)
## 3. z <- makeCacheMatrix(x)
## 4. cacheSolve(z)
##     - returns new inverse matrix
## 5. cacheSolve(z)
##     - returns cached inverse matrix
#
# Overview:
#
# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly
#
# This program has 2 functions that are able to cache the potentially time-consuming computation
# of taking the inverse of a matrix, so if/when it is needed again it can be looked up in the 
# cache rather than recomputed over and over again. 
# 
# The functions take advantage of the scoping rules of the R language and  how they can be 
# manipulated to preserve state inside of an R object.
#


# The below function creates a special "matrix" object that can cache its inverse, including:
#  set the value of the matrix
#  get the value of the matrix
#  set the value of the inverse
#  get the value of the inverse
#
# and by using the <<- operator we are able to assign a value to an object in an environment that 
# is different from the current environment. 
#
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    
    list(set = set, 
         get = get, 
         setinversematrix = setinversematrix, 
         getinversematrix = getinversematrix
    )
    
}


# The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# It first checks to see if the inverse has already been calculated (and the matrix has not changed),
# and if so then the cacheSolve function should retrieve the inverse from the cache and skip the
# computation. Otherwise, it calculates the inverse of the matrix and sets the value in the cache 
# via the setinversematrix function.
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinversematrix()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
    
}
