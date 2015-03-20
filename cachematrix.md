## The first function creates a list of functions for a matrix
## that create the inverse of the matrix and cache it.
## the second function then takes the output of function 1
## as an object and returns the cache if found or calculates
## the inverse if the cache does not exist

## creates 4 cached functions for a matrix that can be
## applied to the cachesolve function in a different environment

makeCacheMatrix <- function(x = matrix()) {
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


## Searches for cached inverse of matrix, uses it if found
## and if not found calculates matrix's inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
    m <- x["getinv()"]
    if(is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    print(data)
    m <- solve(data, ...)
    x["setinv(m)"]
    m

    ## Return a matrix that is the inverse of 'x'
}
        

