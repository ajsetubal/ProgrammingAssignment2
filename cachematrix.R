## These functions take advantage of the scoping rules of the R language
## to provide caching the Inverse of a Matrix caculated by solve function.
## It is assumed that the matrix supplied is always invertible.

## Usage example :
## mdat <- matrix(c(1,2,3, 11,12,15,7,7,10), nrow = 3, ncol = 3, byrow = TRUE)
## B<-makeCacheMatrix(mdat)
## cacheSolve(B)
## round(mdat %*% cacheSolve(B),1)  ## checking identity matrix and cahe

## This function creates a special "cache matrix" object
## with set/set methods to allow caching a given matrix its inverse.
## The cache is persisted in the parent environment.
makeCacheMatrix <- function(a = matrix()) {
    s <- NULL
    set <- function(y) {
        a <<- y
        s <<- NULL
    }
    get <- function() a
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes and returns the inverse of the matrix in "cahe matrix" object.
## If the inverse has already been calculated and the matrix has not changed,
## then the inverse is retrieved from cache.
cacheSolve <- function(a, ...) {
    s <- a$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- a$get()
    s <- solve(data, ...)
    a$setsolve(s)
    s
}


