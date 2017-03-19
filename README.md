## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
  

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inv as NULL which will hold the value of matrix inverse
  inv <- NULL
  ## defining the set function to assign new
  set <- function(y) {
    ##value of matrix in parent environment
    x<<-y
    ##reset inv to null when there is a new matrix
    inv<<-NULL
  }
  ## define the get fucntion - returns value of the matrix argument
   get <- function() x  
  ## assigns value of inv in parent environment
   getinverse <- function() inv 
   ## this is needed in order to refer 
   ## to the functions with the $ operator                                                                                 
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
     if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
       }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}

