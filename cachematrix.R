
## makeCacheMatrix creates a list of functions...
## set=set value of matrix, get=get value of matrix, getInverse=get value of inverse, setInverse=set value of inverse

makeCacheMatrix<-function(x=matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) i<<-inverse
  getInverse <- function() i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## cacheSolve checks to see if inverse of matrix has already been calculated...
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
## if the inverse exits...
  if(!is.null(i)) {
    message("getting cached matrix")
##it returns cached inverse...
    return(i)
  }
  
## if not it calculates the inverse...
  data <- x$get()
  i <- solve(data)
## then caches it...
  x$setInverse(i)
## then returns it.
  i
}
