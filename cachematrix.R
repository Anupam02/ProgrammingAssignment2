## Program to use cached value of Inverse operation in Matrix
#
## makeCacheMatrix -> used to create a Matrix and cache its inverse
#
## cacheSolve -> used to get the cached Value of Matrix ,
##               created by makeCacheMatrix

## makeCacheMatrix function is taking an matrix and saves it's cached value
## every time it gets called

makeCacheMatrix <- function(x = matrix()) {
    library(MASS)
    inv = NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinv <- function(ginv) inv <<- ginv

    getinv <- function() inv

    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


## cacheSolve is used to get the cached value of Inverse of a Matrix,
## created by makeCacheMatrix,
## which is a complex operation which will save the running time
## for going through the Inverse calculation every single time 

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

  ## if there is cached Matrix it will return the cached value of its Inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
    
  }

  ##if inverse is not calculated in makeCacheMatrix (setinv) ,
  ## it will calculate the inverse of it
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinv(inv)
  inv
}
