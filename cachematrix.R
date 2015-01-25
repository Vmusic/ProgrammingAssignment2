## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x) {
      ## create and return a matrix that can cache it's own inverse
      ## this function's job is NOT to create the inverse of a matrix
      ## in creating a special matrix this function will create it with TWO special function 
      ## 1) a function that can cache an inverse matrix passed to it
      ## 2) a function that can return what's been cached
  
      ## arguments REQUIRED "normal" matrix 
      
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
  
      setInverse <- function(inverseMatrix) m <<- inverseMatrix
  
      getInverse <- function() m
  
      list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
      ##first we get or attempt to get the inverse matrix from the special function      
      m <- x$getInverse()
  
      ##now test to see if what was retrieved is NULL, that is the inverse matrix was never cached
      if(!is.null(m)) {
            message("getting cached data")
            return(m)       ## the function exists here
      }
      
      ## the colde below ONLY executes if the getInverse() function call returned NULL
      ## since the inverse isn't set in cache we must create the inverse and set it in cache
      data <- x$get()         ##to create the inverse matrix, we need the matrix data
      
      m <- solve(data, ...)   ## the built in r function solve() will create the inverse
  
      x$setInverse(m)         ##now cache the created inverse matrix using the setInverse 
      
      m                       ##return the inverse matrix
}
