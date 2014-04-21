## makeCacheMatrix is very much similar to the makeVector function provided
## the difference is that x is the matrix and im is the inverse

## cacheSolve is similar to cachemean function
## the difference is that x is the matrix and im is the inverse which is computed
## by using the solve() function

## im (the inverse matrix) is assumed as NULL
## inside we have get, set as getter and setter of the matrix
## we also have getinverse, setinverse which acts as getter and setter of the inverse
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## im (the inverse matrix) is first initialized by running getinverse on the matrix x
## if there is cached inverse, we will immediately return it
## otherwise, we will use solve() to calciulate and then set it back to the matrix x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  the_matrix <- x$get()
  im <- solve(the_matrix, ...)
  x$setinverse(im)
  im
}
