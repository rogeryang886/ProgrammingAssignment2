## Create the matrix, and calculate the inverse of matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. set inv an empty matrix with same dimension

makeCacheMatrix <- function(x = matrix()) {
inv <- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
set <- function(y){
    x <<- y
    inv <<- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
}
get <- function() x
set_inverse <- function(invert) inv <<- invert
get_inverse <- function() inv

list(set = set, get = get,
     set_inverse = set_inverse,
     get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above and set the inverse.
## If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(length(inv) == sum(!is.na(inv))){
        message("existing cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$set_inverse(inv)
    inv
}
