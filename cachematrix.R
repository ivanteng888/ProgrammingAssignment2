## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## First, create a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function() return(inverse);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## Now, compute the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated the "matrix" remains unchanged,
## then "cacheSolve" will produce the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    inverse <- solve(data, ...)
    mtx$setinverse(inverse)
    return(inverse)
}
