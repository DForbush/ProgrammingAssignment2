## This function caches the inverse of a matrix, and if the inverse has not already been calculated, it calculates it. 

makeCacheMatrix <- function (x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    ## I expanded the names of the objects to make more sense to me
    list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
           ## pretty much does the same thing the makeVector function except instead of
           ## taking the mean, it takes the inverse. 
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## first it checks to see if m already exists. If it does, it returns cached data.
    mat.data <- x$get()
    m <- solve(mat.data, ...)
    x$setInverse(m)
    return(m)
    ##if m does not exist, it will "solve" the matrix to produce the inverse. 
}

