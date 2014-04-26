## These two functions are used to effeciently and with caching 
## facilitate the return of the inverse of a square matrix
## a list of set and get functions are used to accomplish this

## makeCacheMatrix takes in a square matrix and sets up a list that gives
## access to set or get the matrix and its inverse
## this list is a special matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes in a special matrix from makeCacheMatrix and using its
## functions sets the inverse and saves it to cache or gets it from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
