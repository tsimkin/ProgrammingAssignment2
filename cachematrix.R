## The function "makeCacheMatrix" will create a list that will track
## if an inverse has been calculated and the functions necessary to
## calculate the inverse.  The function "cacheSolve" will retrieve the
## cached solution if it exists, or calculate the inverse if it does not.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set=set, get=get, setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Alread calculated -- getting cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
