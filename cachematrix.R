## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a special matrix that is actually a list of functions to
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve fetches the inverse from a special matrix x and checks if it is NULL.
## If it's NULL it computes the inverse, saves it in the cache and returns it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    i
}
