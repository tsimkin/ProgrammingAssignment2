## The function "makeCacheMatrix" will create a list that will track
## if an inverse has been calculated and the functions necessary to
## calculate the inverse.  The function "cacheSolve" will retrieve the
## cached solution if it exists, or calculate the inverse if it does not.

# This function takes as an argument a matrix, and creates meta-data about the
# matrix in the form of a list.  The list contains functions necessary to
# compute the inverse of the matrix.

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

# This function returns the inverse of a matrix.  It first looks to see if an
# inverse has already been calculated and cached, in which case it returns the 
# cached valued.  Otherwise, it solves the matrix to return the index, and stores
# the inverse in a variable 'm' associated with the original matrix.

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
