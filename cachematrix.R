## Creation, storage and inversion of matricies
##      Functions:
##          makeCacheMatrix()
##          cacheSolve()
##      Usage: 
##          create new matrix cache (named MAT) from a square matrix (matrix) 
##              MAT <- makeCacheMatrix(matrix)
##          create and cache (retrieve, if already computed) the matrix inverse
##              cacheSolve(MAT)

# Creates an object that can contain a matrix and its inverse
#      values:
#          mat: the stored matrix
#          inv: the inverted matrix
#      method calls:
#          set -- stores matrix (calls set_mat)
#          get -- retrieves stored matrix (calls get_mat)
#          set_inverse -- stores the matrix inverse (calls set_inv)
#          get_inverse -- retrieves the stored matrix inverse (calls get_inv)
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    
    set_mat <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    
    get_mat <- function() mat
    
    set_inv <- function(z) { inv <<- z }
    
    get_inv <- function() inv
    
    list(set = set_mat,
         get = get_mat,
         set_inverse = set_inv,
         get_inverse = get_inv)
}


# Returns and stores (or retrieves) the inverse of a matrix stored in a
#   'makeCacheMatrix' object
cacheSolve <- function(x, ...) {
    if (!is.null(inv <- x$get_inverse())) {
        message("retrieved cached inverse")
        return(inv)
    }
    
    x$set_inverse(solve(x$get(), ...))
    x$get_inverse()
}