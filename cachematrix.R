## Put comments here that give an overall description of what your
## functions do


## These two functions create a special matrix object that can 1. set the input
## matrix, 2. get the input matrix, 3. set the inverse matrix, 4. get the 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL   # initialise an inverse matrix taking NULL value named "inverse"
  
  set <- function(y) {   # define the set function that can:
    x <<- y   # 1. change the x value in the parent environment by assigning y value (new value)
    inverse <<- NULL   # 2. reset the inverse matrix to NULL
  }
  
  get <- function() x   # return the cached input matrix
  
  setinverse <- function(solve) inverse <<- solve   # cache the inverse matrix named "solved" from the next function
  
  getinverse <- function() inverse   # return the inverted matrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # return a list with four options
  
}


cacheSolve <- function(x, ...) {

  inverse <- x$getinverse() # get the cached inverse matrix from a special matrix created with the function above and store it as "inverse"
  
  if(!is.null(inverse)) {   # if there is an inverse matrix
    
    message("getting cached data")   # print out a reminder stating fetching the cached inverse matrix
    
    return(inverse)   # return the cached value
    
  }
  
  data <- x$get()   # if cached value is NULL, get the input matrix and feeds into "data"
  
  inverse <- solve(data, ...)   # work out the inverse matrix using "data"
  
  x$setinverse(inverse)   # store the inverse matrix into "x" using "setinverse"
  
  inverse   # return the inverse matrix 
  
}

