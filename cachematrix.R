## These functions take a matrix and get it's inverse, if it is not given, in an special 
## list. The matrix and it's inverse get stored in the list so they can be easily accessed
## by the $ operator.

## First function: creates a functional list that stores a matrix and it's inverse, if given.
## If there's no argument, a list of four functions is created, ready to store the needed data:
## set: sets the matrix if it wasn't given in the argument.
## get: displays the stored matrix in the list.
## set_inv: sets the inverse of the matrix if it is available.
## get_inv: displays the inverse of the matrix given.

makeCacheMatrix <- function(x = matrix()) {
    b <- NULL
    set <- function(y) {
        x <<- y
        b <<- NULL
    }
    get <- function() {
        x
    }
    set_inv <- function(inv_x) {
        b <<- inv_x
    }
    get_inv <- function() {
        b
    }
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Second function: given the functional list created with "makeCacheMatrix", it reads the 
## list, if the inverse of a matrix was given in the list, then a message is given and the 
## matrix is displayed. Else, it is calculated first, stored in the list with the help of 
## it's third element and then it is displayed.

cacheSolve <- function(x, ...) {
        b <- x$get_inv()
        if(!is.null(b)) {
            message("getting cached data")
            return(b)
        }
        data <- x$get()
        b <- solve(data, ...)
        x$set_inv(b)
        b
}
