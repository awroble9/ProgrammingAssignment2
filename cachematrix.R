## This program caches the inverse of a matrix then provides another function to solve
## the inverse while checking to see if it was solved previously

## The first function, makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
## 1. set the data of the matrix
## 2. get the data of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
# holds cached value or NULL if nothing cached
# to start, set to NULL
      cache <- NULL
      
# store matrix
      setMatrix <- function(newMatrix) {
            x <<- newMatrix
            
# clear the cache
            cache <<- NULL
      }
      
# get stored matrix
      getMatrix <- function() {
            x
      }
      
# cache argument
      cacheInverse <- function(solve) {
            cache <<- solve
      }
      
# get cached value
      getInverse <- function() {
            cache
      }
      
# return list. Each named element is function
      list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}



## The following function calculates the inverse of the special "matrix" created with 
## the above function. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache 

cacheSolve <- function(y, ...) {
      
# get cached value
      inverse <- y$getInverse()
      
# if cached value present, return it
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
# otherwise get matrix, caclulate inverse, and store in cache
      data <- y$getMatrix()
      inverse <- solve(data)
      y$cacheInverse(inverse)
      
# return inverse
      inverse
}