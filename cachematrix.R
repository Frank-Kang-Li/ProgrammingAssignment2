## This function creates a special list that cache the value of input matrix x,
## and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    # set the initial value of inv to NULL
    inv <- NULL
    # 'set' fuction is to set x to the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the value of x
    get <- function() x
    # calculate the inverse of 
    setinv <- function(inverse) inv <<- inverse
    # get the inverse matrix
    getinv <- function() inv
    # make a list including four functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Return a matrix that is the inverse of the 'special' list, 
## if the inverse matrix is already been caculated, 
## then skip the caculation and return the cached inverse matrix.
## if the inverse matrix is not caculated,
## then caculate the inverse matrix and store it in the cache.


# assume the input matrix x is invertible
cacheSolve <- function(x, ...) {
    # set the value of inv to the inverse matrix stored in the 'special' list
    inv <- x$getinv()
    # check if the inverse matrix has already been caculated, if it is
    # caculated then obtain this cache
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    # if it hasn't been caculated then caculate the inverse matrix
    data <- x$get()
    inv <- solve(data)
    # store the inverse into cache
    x$setinv(inv)
    # display the inverse matrix
    inv
        
}
