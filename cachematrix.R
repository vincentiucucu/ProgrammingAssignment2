## The following functions allow the user to cache the inverse of a matrix 
## and retrieve it if it has already been calculated

## Key assumption: matrix supplied is always invertible

## makeCacheMatrix: creates special "matrix" obj. that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
                          mi <- NULL
                          set <- function(y) {
                                  x <<- y
                                  mi <<- NULL
                          }
                          get <- function() {x}
                          setInv <- function(solve) {mi <<- solve}
                          getInv <- function() {mi}
                          list(set = set, get = get,
                               setInv = setInv,
                               getInv = getInv)
}
## Another option above is to use Inverse() from the "matlib" package,
## instead of solve() from the "base" package


## cacheSolve: computes inverse of "special" matrix returned by 
## makeCacheMatrix; if inverse has already been computed and cached,
## it retrieves it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                      mi <- x$getInv()
                      if(!is.null(mi)) {
                              message("Getting cached data")
                              return(mi)
                      }
                      data <- x$get()
                      mi <- solve(data, ...)
                      x$setInv(mi)
                      mi
}
