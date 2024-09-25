## Put comments here that give an overall description of what your
## functions do

## Creates a special 'matrix' object that can cache its inverse and assigned it as makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set m to NULL since there is no calculated value in the cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        ## setinverse function solves the inverse. the return will replace the 'm' variable which is used to solve the function.
        getinverse <- function()m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Create a function that return the inverse and assigned it as cacheSolve. If the inverse has been caculated then return inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        ## uses the setinverse in x to solve the inverse in "m"
        m
}
