## 'makeCacheMatrix' assemble a list of functions that will cache the inverse
## matrix in the environment of the calling function
##
makeCacheMatrix <- function(inputM = matrix()) {
      inverseM <- NULL
      set <- function(y) {
            inputM <<- y
            inverseM <<- NULL
      }
      get <- function() inputM
      setInverse <- function(inverse) inverseM <<- inverse
      getInverse <- function() inverseM
#      get <- function(x) {
#            xx <- inputM
#            eval.parent(substitute(x <- xx))
#      }
#      setInverse <- function(inverse) {
#            inverseM <<- inverse
#      }
#      getInverse <- function(y){
#            yy <- inverseM
#            eval.parent(substitute(y <- yy))
#      } 
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## 'cacheSolve' calculates the inverse of a matrix and caches the inverse
## for future use. If cache is not reset since the last call, the function
## returns the cached inverse rather than repeat the calculation
## 
## Inverse Definition:  A matrix when multiplied by its inverse, produces the
##    the 'identify matrix' (diagonal values '1', all other 
##    values are zero). To have an inverse, a matrix must be... 
##    (1) square, and
##    (2) invertable
##
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- NULL
      data <- NULL
#      x$getInverse(m)
      m <- x$getInverse()
      if(!is.null(m)){
            message("Getting cached data")
            return(m)
      }
#      x$get(data)
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}

test1 <- function(){
      
      InX <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
      InM <- makeCacheMatrix(InX)
      InM$set(InX)
      OutY <- cacheSolve(InM)
      
      InX <- matrix(c(4,3,3,2), nrow=2, ncol=2)
      InM$set(InX)
      OutY <- cacheSolve(InM)
      OutY
            
}