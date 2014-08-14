## These 2 functions allow a user to cache the result of 
## a matrix inversion operation. Sample usage:
##
##  x <- matrix(1:4,nrow=2,ncol=2)
##  m <- makeCacheMatrix(x);
##  i <- cacheSolve(m)
##  ix <- cacheSolve(m)
##
## The first call to "cacheSolve" calculates the inverse 
## of the matrix and then caches it. The second call to  
## "cacheSolve" returns the cached value, without recalculating
## the inverse of the matrix


## This function wraps a matrix with a "cacheable" result
## of the matrix invert operation. It returns a function
## that will allow user to get and set both the underlying
## matrix as well as the inverted matrix. Input must be a
## square matrix
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y){
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  identical <- function(y) identical(x,y)
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## Given a "makeCacheMatrix" function, the cacheSolve function
## will return the inverse of the underlying matrix. If the 
## inverse function has already been called and was cached, it
## is returned. Otherwise the "solve" method is run and cached.
cacheSolve <- function(x, ...) {
  ix <- x$getinverse()
  if(!is.null(ix)){
    message("getting cached data")
    return(ix)
  }
  ix <- solve(x$get())
  x$setinverse(ix)
  ix
}
