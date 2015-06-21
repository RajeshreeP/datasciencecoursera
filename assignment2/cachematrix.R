## https://github.com/RajeshreeP/datasciencecoursera/assignment2
## 1st commit SHA-1 hash identifier: 

## makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## i will store the inverse
    i <- NULL

    ## set used to alter th matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
     
    ## get simply returns the raw matrix
    get <- function() x
    setinverse <- function(inverse) i <<- inverse

    ##getinv gets the cached inverse
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Calculate the inverse of the special "matrix" the special "matrix"
## which created with the makeCacheMatrix function,
## Reusing cached result if it is available 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
makeCacheMatrix <- function(x = matrix()) {
    ## i will store the inverse
    inv <- NULL

    ## set should be used to alter the matrix
    ## it invalidates the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## get simply returns the raw matrix
    get <- function() {
        x
    }

    ## setinv sets the inv variable
    ## should be used only by cacheSolve
    setinv <- function(i) {
        inv <<- i
    }

    # getinv gets the cached inverse
    getinv <- function() {
        inv
    }

    ## return the special matrix
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## get the cached inverse
    inv <- x$getinv()

    if(!is.null(inv)) {
        ## if the inverse if actually cached, just return it
        message("getting cached inverse")
        return(inv)
    }

    ## otherwise, calculate the inverse and cache it
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv)

    return(inv)
}




