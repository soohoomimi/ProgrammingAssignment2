## The two functions coded below allow a matrix object and its inverse to be 
## cached.

## The makeCacheMatrix function takes a given matrix and enables it to
## cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                      # creates an empty variable "m" that acts as a cache  
  set <- function(y) {           # set() allows the user to assign x to a new matrix object, y
    x <<- y                      # content of y is assigned to x     
    m <<- NULL                   # stores matrix object into the cache
  }
  get <- function() x                           # returns the content of the matrix, x
  setinverse <- function(solve) m <<- solve     # calculates the inverted matrix and stores it in "m"
  getinverse <- function() m                    # identifies the content of the inverted matrix supplied by "m"
  list(set = set, get = get,                    # wraps up all 4 functions into a list that is
       setinverse = setinverse,                 # passed through when makeCacheMatrix is called
       getinverse = getinverse)

}

##  The cacheSolve function computes the inverse of the matrix object returned by 
##  makeCacheMatrix above. If the inverse is already calculated and stored in,
##  the cache "m", then cacheSolve retrieves the inverse from the cache. If the
##  inverse is not found, then cacheSolve calculates the inverse and stores the 
##  result into the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()               # searches for x's inverted matrix compuation in the cache
  if(!is.null(m)) {                 # if x's inverse is found in the cache
    message("getting cached data")
    return(m)                       # the cache for x is returned                     
  }                                 # if x is not found in the cache
  data <- x$get()                   # the inverse of x is computed on the fly 
  m <- solve(data, ...)             
  x$setinverse(m)                   # the result is stored back in the cache
  m                                 # returns the matrix inversion result

}


