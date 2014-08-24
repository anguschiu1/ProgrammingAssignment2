## Put comments here that give an overall description of what your
## functions do
# The cachematrix.R serves two propose:
# 1. To contruct a list of functions which is to store a matrix and the inverse of 
# the stored matrix
# 2. To construct a function, the cacheSolve function, to calculate the inverse 
# matrix of a makeCacheMatrix object, and store into that object. This function
# can detect pre-existing calculated results and reuse the inverse matrix instead
# of re-calculation every time.

## Write a short comment describing this function

# makeCacheMatrix function is to construct a list to store a matrix,an inverse of 
# the stored matrix, and a getter and setter function. 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinvm <- function(inputtedResult) inverse <<- inputtedResult
    getinvm <- function() inverse
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## Write a short comment describing this function
# The cacheSolve function calculates the inverse 
# matrix of a makeCacheMatrix object, and store into makeCacheMatrix 
# list object.
cacheSolve <- function(x, ...) {
    m <- x$getinvm()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvm(m)
    ## Return a matrix that is the inverse of 'x'
    m
}