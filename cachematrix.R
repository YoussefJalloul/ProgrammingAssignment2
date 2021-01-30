## makeCacheMatrix and cacheSolve allow you to store, edit, and retrieve
## the value of a matrix or its inverse

## makeCache Matrix creates a list of functions that allow you to
## store (when calling the function), edit (using the set functions),
## and retrieve (using the get functions) the value of a matrix (set and get)
## and its inverse counterpart (setinv & getinv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverseMatrix) inv <<- inverseMatrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the function and stores it
## in makeCacheMatrix's cache
## If the inverse is already present (from a previous calculation),
## it simply retrieves its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
