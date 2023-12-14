## The functions makeCacheMatrix and cacheSolve 
## are used in conjunction to create special list containing functions to manipulate a matrix and its inverse
##
## how I tested:
##
## test1:
## m1 <- makeCacheMatrix()
## m1$set (matrix(c(2, 1, 3, 4), nrow = 2, ncol=2))
## cacheSolve (m1)
## cacheSolve (m1)
##
## test2:
## m2  <- makeCacheMatrix()
## m2$set (matrix(c(1, 2, 2, 3), nrow = 2, ncol=2))
## cacheSolve (m2)


## makeCacheMatrix
## creates a structure (list) that has the functions set, get, setinv, getinv
## stores the original matrix in the m variable
## stores the inverse in the inv variable
## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(mm) {
                m <<- mm
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(pInv) inv <<- pInv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve
## if the inverse exists, return it
## otherwise, it calculates the inverse and set it using x$setinv(inv)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
