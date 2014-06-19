## Creation, storage and inversion of matricies
##      Functions:
##          makeCacheMatrix()
##          cacheSolve()
##      Usage: 
##          create new matrix cache (named MAT) from a square matrix (matrix) 
##              MAT <- makeCacheMatrix()
##              MAT$set(matrix)
##          create and cache (retrieve, if already computed) the matrix inverse
##              cacheSolve(MAT)
##      Warning:
##          multiple makeCacheMatrix objects will all alter the same variables
##              so you can only use one at a time 

# Creates an object that can contain a matrix and its inverse
#      values:
#          mat: the stored matrix
#          inv: the inverted matrix
#      methods:
#          set -- stores matrix
#          get -- retrieves stored matrix
#          set.inverse -- stores the matrix inverse
#          get.inverse -- retrieves the stored matrix inverse 
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


# Returns and stores (or retrieves) the inverse of a matrix stored in a
#   'makeCacheMatrix' object
cacheSolve <- function(x, ...) {
    if (!is.null(inv <<- x$get.inverse())) {
        message("retrieved cached inverse")
        return(inv)
    }
    
    x$set.inverse(solve(x$get(), ...))
    x$get.inverse()
}