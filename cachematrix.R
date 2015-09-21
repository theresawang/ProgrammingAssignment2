## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        
}

##test
## A <- matrix(c(2,2,3,2),2,2)
## m = makeCacheMatrix(A)
## m$get()
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## cacheSolve(m)
##       [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
