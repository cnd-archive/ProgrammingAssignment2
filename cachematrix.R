# The purpose of these functions is to make a cache-able version of a matrix
# so that when the matrix's inverse is calculated, that calculation is saved.

## Given a matrix, create a list of functions that manipulate that matrix.
## Specifically, add functions to save the inverse of the matrix.
## The period (.) in front of .inverse is a convention we use for designating
## variables we are storing information in. The way our function works below
## is very much like creating an object; closures have been called "a poor
## man's objects" before. Given that, the period designates .inverse as a
## private variable, only to be accessed through its getter and setter.

makeCacheMatrix <- function(x = matrix()) {
    .inverse <- NULL
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    setinverse <- function (inverse) .inverse <<- inverse
    getinverse <- function () .inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function takes our cacheable matrix we made with makeCacheMatrix
## and gets its inverse. If the inverse has already been calculated, we use
## that; otherwise, we calculate the inverse and save it for subsequent
## uses.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()

    if (is.null(inverse)) {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
    } else {
        message("getting cached inverse")
    }

    inverse
}
