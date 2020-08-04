## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and store it in the cache
makeCacheMatrix <- function(x = matrix()) {
        x <- x
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this function takes a matrix, if the inverse has alerady been calculated, it 
# searches for it in the cache and return the value, instead of recalculating.
# Else, it calculates its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}