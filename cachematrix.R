## This function will (1) take a matrix input and create a cache version of it, 
 #where it will be possible to store its inverse matrix and (2) actually calculates the inverse
 #using the SOLVE function, then stores its value in cache. If the value has already been computed
 #it will be recalled from cache on a second occasion

makeCacheMatrix <- function(x = numeric()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
