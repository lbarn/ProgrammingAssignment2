## makeCacheMatrix to cache inverse of matrix 
## inv= value to solve for (inverse)
## set with <<- operator for to assign a value in the parent envirnoment 
## inv set as NULL, will be defined later in the function 
## get to define getter for x 
## setinv to calculate inverse with solve function 
## getinv to define getter for inv
## list to name each element in the funtion 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cachesolve to populate and retrieve inverse of matrix X
## from makeCacheMatrix and   
## call getinv function 
## check if inv has already been defined in environment
## if inv is not NULL, get matrix and calculate inverse with solve 

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

