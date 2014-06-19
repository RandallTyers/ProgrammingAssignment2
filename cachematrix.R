## Put comments here that give an overall description of what your
## functions do

## Creates an object that can contain a matrix and its inverse
##      values:
##          mat = the stored matrix
##          inv = the inverted matrix
##      methods:
##          set -- store matrix
##          get -- retrieve stored matrix
##          set.inverse -- store the matrix inverse
##          get.inverse -- retrieve the stored matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    
    get <- function() mat
    
    set.inverse <- function(z) { inv <<- z }
    
    get.inverse <- function() inv
    
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Returns the inverse of a matrix stored in a 'makeCacheMatrix' object

cacheSolve <- function(x, ...) {
    if (!is.null(inv <<- x$get.inverse())) {
        message("retrieved cached inverse")
        return(inv)
    }
    
    x$set.inverse(solve(x$get(), ...))
    x$get.inverse()# -> output
    #output
}














#### examples ####

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


##### play

makeVector2 <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function() m <<- mean(x)
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
