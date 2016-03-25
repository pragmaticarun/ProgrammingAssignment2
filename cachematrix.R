#author Arunkumar Maniam Rajan

##  This R program contains functions which creates a special 
##  matrix object which can cache the inverse.

## This function provides set/get methods for modifying the matrix 
## and provides setInverse/getInverse functions to set/get the cache
## which holds the Inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      cachedInverse <- NULL
      set <- function(newMatrix) {
         x <<- newMatrix
         cachedInverse <<- NULL #Needs to be done if a Matrix changed
      }

      get <- function() {
         x
      }

      setInverse <- function(inverse) {
         cachedInverse <<- inverse
      }

      getInverse <- function() {
         cachedInverse
      }

      list( set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse )
         
}


## This function caches the result of inverse operation in the 
## object passed which can store the inverse.

cacheSolve <- function(x, ...) {
   i = x$getInverse()
   if(!is.null(i)) {
      message("Getting cached Inverse")
      return(i)
   }

   inputMatrix = x$get()
   i <- solve(inputMatrix, ...)
   x$setInverse(i)
   i
}

## Test function to generate invertible matrix
hilbert <- function(n){ 
   i <- 1:n; 1 / outer(i - 1, i, "+") 
}

