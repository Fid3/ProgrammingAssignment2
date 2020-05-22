## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix store the matrix.
## This function brings back a list with 5 functions.
## Among these, the inverse matrix function.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        inverse <- function(x) solve( x, diag(nrow(x)))
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        list(set = set, get = get, inverse = inverse,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## The function cacheSolve verify if the inverse matrix has been
## calculated. If the answer is yes, the function brings back the
## stored data. If not, the inverse is calculated.
    cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- x$inverse(data, ...)
            x$setinverse(m)
            m
    }
