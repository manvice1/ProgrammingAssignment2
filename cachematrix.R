## The below function is designed to create a matrix 
## that can cache its inverse. It sets and the gets the matrix,
## then setting the inverse and getting the inverse.

makeCacheMatrix <- function(x = matrix()) { ## define argument as a matrix
  inver <- NULL ## set inv as NULL - matrix inverse value
  set <- function(y) {
    x <<- y ## assign new value of matrix
    inver <<- NULL ## reset to NULL if new matrix
  }
  get <- function() x ## designed to return matrix value
  setinverse <-function(inverse) inver <<- inverse ## assign value of inv in overall environment
  getinverse <- function() inver ## resolves inv 
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse) ## loops over the above functions
}

## This function calculates the inverse of the matrix define above
## If already calculated the inverse is recalled from the cache

CacheSolve <- function(x,...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("Cached - fetching data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data,...)
  x$setinverse(inver)
  inver
}
