## Creation, storage and inversion of matricies
##      Functions:
##          makeCacheMatrix()
##          cacheSolve()
##      Usage: 
##          create new matrix cache (named MAT) from a matrix (matrix) 
##              MAT <- makeCacheMatrix(matrix)
##          create and cache (retrieve, if already computed) the matrix inverse
##              cacheSolve(MAT)
##      Warning:
##          assumes that the matrix entered is invertable


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
    # error checking -- make empty object if invalid argument
    stop_msg = "argument must be an invertible square matrix"
    if (invalid.matrix(mat)) stop(stop_msg)
    
    # initalize inv as NULL
    inv <- NULL
    
    # called using 'set'
    #   replaces stored matrix (if any) with a new matrix
    #   NB: entering an invalid matrix results in deletion of the object
    set_mat <- function(y) {
        # error checking
        if (invalid.matrix(y)) {
            # better to have nothing than the wrong thing ...
            rm(list=c(strsplit(x=as.character(grep("$", sys.calls(), value=T, fixed=T)), split="$", fixed=T)[[1]][1]), envir=.GlobalEnv)
            stop(paste(stop_msg, "-- object deleted"))
        }
        
        mat <<- y
        inv <<- NULL
    }
    
    # called using 'get'
    #   retrieves stored matrix
    get_mat <- function() mat
    
    # called using 'set_inverse'
    #   inserts (if valid) a matrix
    set_inv <- function(z) {
        # error checking
        if (invalid.matrix(mat)) stop(paste(stop_msg, "-- inverse matrix not set"))
        inv <<- z
    }
    
    # called using 'get_inverse'
    #   retrieves stored inverse of matrix or NULL if not set
    get_inv <- function() inv
    
    list(set = set_mat,
         get = get_mat,     # NB: get is an R command this avoids overriding it
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

# Checks whether argument is a valid matrix
invalid.matrix <- function(mat) {
    if (!is.matrix(mat)) {
        message("Invalid argument -- not a matrix")
        return(TRUE)
    }
    # check whether this matrix is square (2D, equal dimensions)
    dims = dim(mat)
    if (length(dims) != 2 || dims[1] != dims[2]) {
        message("Invalid argument -- matrix is not square")
        return(TRUE)
    }
    
    # check whether square matrix is invertable
    if (det(mat) == 0) {
        message("Invalid argument -- square matrix is not invertible")
        return(TRUE)
    }
    
    # passed the tests so it seems to be a valid matrix (i.e. not invalid)
    return(FALSE)
}