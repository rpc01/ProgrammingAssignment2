## This script computes the inverse of a matrix. Optimizes CPU usage by using the inverse in case already 
## exists in the global environment, otherwise uses the solve function to compute it. The routine takes advantage 
## of the R lexical scoping, using the <<- operator to store values in the global environment, and <- to use in the
## local environment.

## makeCacheMatrix creates a list object of four functions:
## 1 set: to enter a matrix to invert and store it in the global.env 
## 2 get: to obtain the matrix from glob.env
## 3 setinverse: to compute the matrix inverse and store it in the global.env 
## 4 getinverse: to obtain the matrix inverse from glob.env

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes, stores in glob.env and returns the inverse of a matrix.
## if the matrix has a $getinverse() object not null, uses the inverse stored in the glob.env, otherwise 
# uses solve to compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
