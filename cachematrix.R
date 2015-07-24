## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invx <- matrix()
    set <- function(y) {
        x <<- y
        invx <<- matrix()
    }
    get <- function() x
    setInv <- function(inv_mat) invx <<- inv_mat
    getInv <- function() invx
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getInv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setInv(invx)
        invx
}
