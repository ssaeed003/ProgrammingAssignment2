## These functions help store the inverse of a matrix in cache such that the inverse would not have to be calculated again


## This funciton takes as an argument one matrx and if nothing is given then creates one as default and returns a list 
## of methods from which the inverse and matrix can be retreived and set 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## This function returns the inverse of a matrix created by makeCacheMatrix function by calculating it if nothing is stored 
## in cache or just directly retrieving it from cache if it exists.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <-solve(data,...)
  x$setinverse(i)
  i
}
