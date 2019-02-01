## set matrix, get matrix, set inverse of a matrix, get inverse of a matrix

## sets a matrix and gets a stored matrix, sets and gets the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setINV <- function(inverse) m <<- inverse
    getINV <- function() m
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)
}

#returns an inverse of a matrix, takes a cached value if available, 
#otherwice creates a new one
cacheSolve <- function(x, ...) {
    m <- x$getINV()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setINV(m)
    m
}