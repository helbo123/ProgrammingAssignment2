## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix returns a list containing four functions representing the input-matrix.
## Use of the functions:
## get() for returning the matrix
## set() for setting a new matrix
## set_inverse() for setting the inverse of the matrix (only setting of the inverse, no calculation here)
## get_inverse() for returning the inverse of the matrix (only if it already exists)
makeCacheMatrix <- function(mat_x = matrix()) {
    m <- NULL
    set <- function(mat_y) {
        mat_x <<- mat_y
        m <<- NULL
    }
    get <- function() mat_x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse) 
}


## cacheSolve calculates sets and returns the inverse of the mat_x matrix stored to the special list via makeCacheMatrix
## if the inverse has been calculated and stored already to the special list, it will print "getting cached inverse matrix"
## and returns the cached inverse matrix of mat_x
cacheSolve <- function(mat_x, ...) {
    ## Return a matrix that is the inverse of 'mat_x'
    m <- mat_x$get_inverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    mat_data <- mat_x$get()
    m <- solve(mat_data)
    mat_x$set_inverse(m)
    m
}
