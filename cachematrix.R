makeCacheMatrix <- function(x = matrix()) {
  ##for getting inverse matrix x
  inversx <- NULL 
  set <- function(y) {
    x <<- y
    inversx <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inversx <<-inverse ##converting to inverse using Solve(A)
  getinverse <- function() inversx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  inversx <- x$getinverse()
  if (!is.null(inversx)) {
    message("getting cached inverse matrix")
    return(inversx)
  } else {
    inversx <- solve(x$get())
    x$setinverse(inversx)
    return(inversx)
  }
}
##output
##> a <- matrix(1:4,2,2)
##> tst <- makeCacheMatrix(a)
##> tst$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(tst)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
