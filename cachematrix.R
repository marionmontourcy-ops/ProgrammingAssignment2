## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creat an object "CacheMatrix" : a list of 4 functions 
# (set, get, setinverse, getinverse) which share a private environment thanks to <<-. 
# The inverse is saved in inv (initially NULL). It's a closure : each call create
# an independant memory space.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculate the inverse of a makeCacheMatrix object : it checks 
# first the cache via getinverse() ; if the inverse already exists, it returns 
# directy (without calculation) the message "getting cached data". Otherwise, 
# it recover the matrix with get(), calculate the inverse with solve(), stock it 
# with setinverse() and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
